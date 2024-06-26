#' Prepare data input for plotting upstream fossil fuel exposure
#'
#' Prepare data input for plotting upstream fossil fuel exposure based
#' on PACTA for investors output files. These files must have been wrangled with
#' `prep_data_executive_summary()` before they can be passed to this function.
#'
#' @param results_portfolio Data frame that contains pre-wrangled portfolio
#'   level PACTA results from a PACTA for investors analysis.
#' @param peers_results_aggregated Data frame that contains pre-wrangled
#'   aggregate peer group level PACTA results from a PACTA for investors
#'   analysis.
#' @param indices_results_portfolio Data frame that contains pre-wrangled
#'   PACTA results for indices from a PACTA for investors analysis.
#' @param scenario_selected Character. Must be a `scenario` featured in the
#'   `scenario_thresholds` data set. Defaults to `"1.5C-Unif"` from the GECO2021
#'   scenario source.
#'
#' @return data.frame
#' @export
prep_fossil_bars <- function(results_portfolio,
                             peers_results_aggregated,
                             indices_results_portfolio,
                             scenario_selected = "NZE_2050") {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("fossil_bars")
  } else {
    # check input
    check_data_prep_fossil_bars(scenario_selected = scenario_selected)

    # available portfolio asset classes
    available_asset_classes <- unique(results_portfolio$asset_class)

    # combine input data sets
    data <- results_portfolio %>%
      dplyr::bind_rows(peers_results_aggregated) %>%
      dplyr::bind_rows(indices_results_portfolio)

    # infer start year
    start_year <- min(data$year, na.rm = TRUE)

    # filter combined input data
    data <- data %>%
      dplyr::filter(
        .data$year == .env$start_year,
        .data$scenario == .env$scenario_selected,
        .data$ald_sector %in% c("Coal", "Oil&Gas"),
        .data$asset_class %in% .env$available_asset_classes
      )

    # wrangle data into output format
    data_out <- data %>%
      wrangle_data_fossil_bars()
  }
  data_out
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
    dplyr::mutate(
      entity_type = dplyr::case_when(
        .data$entity %in% c("peers", "index") ~ "benchmark",
        TRUE ~ .data$entity_type
      )
    ) %>%
    dplyr::mutate(
      entity_name = dplyr::case_when(
        .data$entity_name == "peers_average" ~ "peers",
        TRUE ~ .data$entity_name
      ),
      entity = dplyr::case_when(
        .data$entity == "this_portfolio" ~ "portfolio",
        TRUE ~ .data$entity
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
    dplyr::select(-c("sector_p4b", "technology_p4b")) %>%
    dplyr::select(
      c("entity", "entity_name", "entity_type", "year", tech = "technology",
      perc_aum = "plan_carsten", "asset_class")
    ) %>%
    dplyr::arrange(.data$asset_class, dplyr::desc(.data$entity_name), .data$tech)

  return(data)
}
