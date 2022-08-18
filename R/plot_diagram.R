#' Create a diagram of asset class coverage
#'
#' @param data A data frame. In principle an output of `prep_diagram()`.
#'   Requirements:
#'   * Must have columns: `asset_class`, `exposure_portfolio`, 
#'   `exposure_asset_class`, `exposure_asset_class_perc`, `exposure_pacta`, 
#'   `exposure_pacta_perc_asset_class_exposure`, `emissions_pacta_perc`, 
#'   `emissions_pacta`.
#'   * Must have two rows.
#'   * `asset_class` must have values: "equity", "bonds".
#'   * `exposure_portfolio`, `exposure_asset_class`, `exposure_asset_class_perc`, 
#'   `exposure_pacta`, `exposure_pacta_perc_asset_class_exposure`, 
#'   `emissions_pacta_perc`, `emissions_pacta` must be numeric.
#'   * `exposure_asset_class_perc`, `exposure_pacta_perc_asset_class_exposure`, 
#'   `emissions_pacta_perc` must be a percentage in decimal format, with values 
#'   between 0 and 1.
#'
#' @return An object of class "htmlwidget".
#' @export
#'
#' @examples
#' plot_diagram(toy_data_diagram)
plot_diagram <- function(data = NULL) {
  env <- list(data = substitute(data))
  check_data_diagram(data, env)
  
  data_bonds <- data %>%
    filter(.data$asset_class == "bonds")
  
  data_equity <- data %>%
    filter(.data$asset_class == "equity")
  
  DiagrammeR::grViz(glue("digraph {
  graph [rankdir = LR]
  
  node [shape = rectangle]  
  
  subgraph cluster_total_exposure {
    peripheries = 0 
  
    label = 'Total Portfolio Exposure'
  
    total_exp [label = '{{formatC(data_equity$exposure_portfolio[1], format='f', big.mark=',', digits=0)}} CHF']
  }
  
  subgraph cluster_asset_class_exposure {
    peripheries = 0
  
    label = 'Exposure per asset class'
  
    equity_exp [label = '{{scales::percent(data_equity$exposure_asset_class_perc[1])}} 
  ({{formatC(data_equity$exposure_asset_class[1], format='f', big.mark=',', digits=0)}} CHF)']
    bonds_exp [label = '{{scales::percent(data_bonds$exposure_asset_class_perc[1])}} 
  ({{formatC(data_bonds$exposure_asset_class[1], format='f', big.mark=',', digits=0)}} CHF)']
  }
  
  subgraph cluster_pacta_exposure {
    peripheries = 0 
  
    label = 'Exposure covered by PACTA sectors*\n(as % of asset class exposure)'
  
    equity_pacta_exp [label =  '{{scales::percent(data_equity$exposure_pacta_perc_asset_class_exposure[1])}} 
  ({{formatC(data_equity$exposure_pacta[1], format='f', big.mark=',', digits=0)}} CHF)']
    bonds_pacta_exp [label = '{{scales::percent(data_bonds$exposure_pacta_perc_asset_class_exposure[1])}} 
  ({{formatC(data_bonds$exposure_pacta[1], format='f', big.mark=',', digits=0)}} CHF)']  
  }

  subgraph cluster_emissions {
    peripheries = 0
  
    label = 'Emissions covered by PACTA sectors*\n(as % os asset class emissions)'
  
    equity_emission_exp [label =  '{{scales::percent(data_equity$emissions_pacta_perc[1])}} 
  ({{formatC(data_equity$emissions_pacta[1], format='f', big.mark=',', digits=0)}} tonnes)']
    bonds_emission_exp [label = '{{scales::percent(data_bonds$emissions_pacta_perc[1])}} 
  ({{formatC(data_bonds$emissions_pacta[1], format='f', big.mark=',', digits=0)}} tonnes)']
  }

  # edge definitions with the node IDs
  total_exp -> equity_exp [label = 'Listed Equity']
  equity_exp -> equity_pacta_exp -> equity_emission_exp
  total_exp -> bonds_exp [label = 'Corporate Bonds']
  bonds_exp -> bonds_pacta_exp -> bonds_emission_exp
  }", .open = "{{", .close = "}}"))
}

check_data_diagram <- function(data, env) {
  stopifnot(is.data.frame(data))
  abort_if_has_zero_rows(data, env = env)
  stopifnot(nrow(data) == 2)
  abort_if_missing_names(
    data,
    c("exposure_portfolio", "asset_class", "exposure_asset_class", 
      "exposure_asset_class_perc", "exposure_pacta", 
      "exposure_pacta_perc_asset_class_exposure", "emissions_pacta_perc", 
      "emissions_pacta")
  )
  abort_if_missing_crucial_values(data, "asset_class", c("equity", "bonds"), env)
  stopifnot(is.numeric(data$exposure_portfolio))
  stopifnot(is.numeric(data$exposure_asset_class))
  stopifnot(is.numeric(data$exposure_asset_class_perc))
  stopifnot(is.numeric(data$exposure_pacta))
  stopifnot(is.numeric(data$exposure_pacta_perc_asset_class_exposure))
  stopifnot(is.numeric(data$emissions_pacta))
  stopifnot(is.numeric(data$emissions_pacta_perc))
  stopifnot((data$exposure_asset_class_perc <= 1) & (data$exposure_asset_class_perc >= 0))
  stopifnot((data$exposure_pacta_perc_asset_class_exposure <= 1) & (data$exposure_pacta_perc_asset_class_exposure >= 0))
  stopifnot((data$emissions_pacta_perc <= 1) & (data$emissions_pacta_perc >= 0))
}