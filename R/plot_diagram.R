plot_diagram <- function(data = NULL) {
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