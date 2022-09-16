prep_fossil_bars <- function(results_portfolio,
                             peers_results_aggregated,
                             indices_results_portfolio,
                             scenario_selected) {
  # check input
  check_data_prep_fossil_bars(scenario_selected = scenario_selected)

  # combine input data sets
  indices_results_portfolio <- indices_results_portfolio %>%
    dplyr::filter(.data$portfolio_name == .env$index_selected_lookup)

  data <- results_portfolio %>%
    dplyr::bind_rows(peers_results_aggregated) %>%
    dplyr::bind_rows(indices_results_portfolio)

  # infer start year
  start_year <- min(data$year, na.rm = TRUE)

  # filter combined input data
  data <- data %>%
    dplyr::filter(
      year == .env$start_year,
      .data$scenario == .env$scenario_selected,
      .data$ald_sector %in% c("Coal", "Oil&Gas")
    )

  # wrangle data into output format
  data_out <- data %>%
    wrangle_data_fossil_bars()

  return(data_out)
}

check_data_prep_fossil_bars <- function(scenario_selected) {
  if (!scenario_selected %in% unique(get("scenario_thresholds")$scenario)) {
    stop("Argument scenario_selected does not hold an accepted value.")
  }
  if (length(scenario_selected) != 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
}

wrangle_data_fossil_bars <- function(data) {
  data <- data %>%
    dplyr::rename(entity_type = .data$entity) %>%
    dplyr::mutate(
      entity_name = dplyr::case_when(
        .data$entity_type == "this_portfolio" ~ "portfolio",
        .data$entity_type == "peers" ~ "peers",
        .data$entity_type == "index" ~ .env$index_short_lookup,
        TRUE ~ .data$entity_type
      )
    ) %>%
    dplyr::mutate(
      entity_type = dplyr::case_when(
        .data$entity_type %in% c("peers", "index") ~ "benchmark",
        TRUE ~ .data$entity_type
      )
    ) %>%
    dplyr::inner_join(
      get("p4i_p4b_sector_technology_mapper"),
      by = c("ald_sector" = "sector_p4i", "technology" = "technology_p4i")
    ) %>%
    dplyr::mutate(
      ald_sector = .data$sector_p4b,
      technology = .data$technology_p4b
    ) %>%
    dplyr::select(-c(.data$sector_p4b, .data$technology_p4b)) %>%
    dplyr::select(
      .data$entity_name, .data$entity_type, .data$year, tech = .data$technology,
      perc_aum = .data$plan_carsten, .data$asset_class
    ) %>%
    dplyr::arrange(.data$asset_class, dplyr::desc(.data$entity_name), .data$tech)

  return(data)
}
