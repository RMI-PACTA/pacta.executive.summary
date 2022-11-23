#' Prepare data input for plotting technology alignment table
#'
#' Prepare data input for plotting technology alignment table based
#' on PACTA for investors output files. These files must have been wrangled with
#' `prep_data_executive_summary()` before they can be passed to this function.
#'
#' @param results_portfolio Data frame that contains pre-wrangled portfolio
#'   level PACTA results from a PACTA for investors analysis.
#' @param peers_results_aggregated Data frame that contains pre-wrangled
#'   aggregate peer group level PACTA results from a PACTA for investors
#'   analysis.
#' @param asset_class Character defining the asset class of the data. Must be
#'   either "equity" or "bonds"
#' @param scenario_source Character. Must be a
#'   `scenario_source` featured in the `scenario_thresholds` data set.
#'
#' @return data.frame
#' @export
prep_alignment_table <- function(results_portfolio,
                                 peers_results_aggregated,
                                 asset_class = c("equity", "bonds"),
                                 scenario_source = "GECO2021") {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("alignment_table") %>% filter(asset_class == .env$asset_class)
  } else {
    # validate inputs
    match.arg(asset_class)
    check_data_prep_alignment_table(scenario_source = scenario_source)

    portfolio_data_asset <- results_portfolio %>%
      filter(asset_class == .env$asset_class)
    if (nrow(portfolio_data_asset) > 0) {
      # infer start_year
    start_year <- min(results_portfolio$year, na.rm = TRUE)

    # filter selected asset_class
    data <- results_portfolio %>%
      dplyr::bind_rows(peers_results_aggregated)

    # get scenarios
    scenario_thresholds <- get("scenario_thresholds")

    scenarios <- scenario_thresholds %>%
      dplyr::filter(.data$scenario_source == .env$scenario_source) %>%
      dplyr::pull("scenario")

    # get scenarios for relevant thresholds
    scenario_high_ambition <- scenario_thresholds %>%
      dplyr::filter(.data$threshold == "high") %>%
      dplyr::pull("scenario")

    scenario_medium_ambition <- scenario_thresholds %>%
      dplyr::filter(.data$threshold == "mid") %>%
      dplyr::pull("scenario")

    # prepare data for technology alignment calculation
    data_tech_alignment <- data %>%
      wrangle_input_data_alignment_table(scenarios = scenarios)

    # calculate technology level alignment
    data_tech_alignment <- data_tech_alignment %>%
      calculate_technology_alignment(
        start_year = start_year,
        time_horizon = time_horizon_lookup
      )

    # calculate the traffic light for each technology
    data_tech_alignment_color <- data_tech_alignment %>%
      calculate_tech_traffic_light(
        scenario_high_ambition = scenario_high_ambition,
        scenario_medium_ambition = scenario_medium_ambition
      )

    # map green/brown categories
    data_tech_alignment_color <- data_tech_alignment_color %>%
      dplyr::mutate(
        green_or_brown = dplyr::case_when(
          .data$green_or_brown == "green" ~ "Low-carbon",
          .data$green_or_brown == "brown" ~ "High-carbon",
          TRUE ~ .data$green_or_brown
        )
      )

    # select the relevant variables
    data_out <- data_tech_alignment_color %>%
      dplyr::select(
        c("ald_sector", "technology", "asset_class", "entity", "aligned_scen_temp",
          "plan_carsten", "green_or_brown")
      ) %>%
      dplyr::rename(
        sector = "ald_sector",
        perc_aum = "plan_carsten",
        green_brown = "green_or_brown"
      ) %>%
      filter(.data$asset_class == .env$asset_class)
    } else {
      data_out <- toy_data_alignment_table[0L, ]
    }
  }
  data_out
}

check_data_prep_alignment_table <- function(scenario_source) {
  if (!scenario_source %in% unique(get("scenario_thresholds")$scenario_source)) {
    stop("Argument scenario_source does not hold an accepted value.")
  }
  if (length(scenario_source) != 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
}

wrangle_input_data_alignment_table <- function(data,
                                               scenarios) {
  # filter data
  data <- data %>%
    dplyr::filter(.data$scenario %in% .env$scenarios)

  # map sectors to p4b style
  data <- data %>%
    dplyr::inner_join(
      get("p4i_p4b_sector_technology_mapper"),
      by = c("ald_sector" = "sector_p4i", "technology" = "technology_p4i")
    ) %>%
    dplyr::mutate(
      ald_sector = .data$sector_p4b,
      technology = .data$technology_p4b
    ) %>%
    dplyr::select(-c("sector_p4b", "technology_p4b"))

  # Coal, Oil & Gas are mapped to Fossil Fuels sector
  data <- data %>%
    dplyr::mutate(
      ald_sector = dplyr::if_else(
        .data$ald_sector %in% c("coal", "oil_and_gas"),
        "fossil_fuels",
        .data$ald_sector
      )
    )

  # Keep only sectors and technologies as defined in the template
  data <- data %>%
    dplyr::filter(
      .data$ald_sector %in% c("automotive", "fossil_fuels", "power"),
      !(.data$ald_sector == "automotive" & stringr::str_detect(.data$technology, "_hdv")),
      .data$technology != "fuelcell",
      !(.data$ald_sector == "power" & .data$technology %in% c("hydrocap", "nuclearcap"))
    )

  return(data)
}

calculate_tech_traffic_light <- function(data,
                                         scenario_high_ambition,
                                         scenario_medium_ambition) {
  data <- data %>%
    dplyr::filter(
      .data$scenario %in% c(.env$scenario_high_ambition, .env$scenario_medium_ambition)
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$scenario, .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector, .data$technology
    ) %>%
    dplyr::mutate(
      tech_score = dplyr::case_when(
        .data$scenario == .env$scenario_high_ambition & .data$tech_trajectory_alignment > 0 ~ 10,
        .data$scenario == .env$scenario_medium_ambition & .data$tech_trajectory_alignment > 0 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity_name, .data$entity_type, .data$entity, .data$scenario_source,
      .data$allocation, .data$equity_market,
      .data$scenario_geography, .data$ald_sector, .data$technology
    ) %>%
    dplyr::mutate(
      aligned_scen_temp = dplyr::case_when(
        sum(.data$tech_score) >= 10 ~ "<1.5\u00B0C",
        sum(.data$tech_score) == 1 ~ "1.5-1.8\u00B0C",
        TRUE ~ ">1.8\u00B0C"
      ),
      entity = replace(.data$entity, .data$entity == "this_portfolio", "portfolio")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$scenario == .env$scenario_high_ambition)

  return(data)
}
