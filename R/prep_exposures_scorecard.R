#' Prepare data input for plotting exposure chart on the climate score card
#'
#' Prepare data input for plotting exposure chart on the climate score card
#' based on PACTA for investors output files. These files must have been
#' wrangled with `prep_data_executive_summary()` before they can be passed to
#' this function.
#'
#' @param results_portfolio Data frame that contains pre-wrangled portfolio
#'   level PACTA results from a PACTA for investors analysis.
#' @param scenario_selected Character. Must be a `scenario` featured in the
#'   `scenario_thresholds` data set. Default is "1.5C-Unif", which is the 1.5°C
#'   scenario from GECO2021.
#'
#' @return data.frame
prep_exposures_scorecard <- function(results_portfolio,
                                     scenario_selected = "1.5C-Unif") {
  # check input
  check_data_prep_exposures_scorecard(scenario_selected = scenario_selected)

  # input data
  data <- results_portfolio

  # infer start year
  start_year <- min(data$year, na.rm = TRUE)

  # filter data
  data <- data %>%
    dplyr::filter(
      year == .env$start_year,
      .data$scenario == .env$scenario_selected
    )

  # calculate current exposures and wrangle for score card
  data_out <- data %>%
    wrangle_data_exposures_scorecard()

  return(data_out)
}

check_data_prep_exposures_scorecard <- function(scenario_selected) {
  if (!scenario_selected %in% unique(get("scenario_thresholds")$scenario)) {
    stop("Argument scenario_selected does not hold an accepted value.")
  }
  if (length(scenario_selected) != 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
}

wrangle_data_exposures_scorecard <- function(data) {
  data <- data %>%
    dplyr::inner_join(
      get("p4i_p4b_sector_technology_mapper"),
      by = c("ald_sector" = "sector_p4i", "technology" = "technology_p4i")
    ) %>%
    dplyr::mutate(
      ald_sector = .data$sector_p4b,
      technology = .data$technology_p4b
    ) %>%
    dplyr::mutate(
      sector_or_tech = dplyr::case_when(
        .data$ald_sector == "coal" ~ "coal",
        .data$ald_sector == "oil and gas" ~ "other_fossil_fuels",
        .data$technology %in% c("coalcap", "gascap", "oilcap") ~ "fossil_power",
        .data$technology == "renewablescap" ~ "renewables_power",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(.data$sector_or_tech)) %>%
    dplyr::select(c(asset_class, sector_or_tech, plan_carsten)) %>%
    dplyr::rename(exposure_perc_aum = plan_carsten) %>%
    dplyr::group_by(asset_class, sector_or_tech) %>%
    dplyr::summarise(
      exposure_perc_aum = sum(exposure_perc_aum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()
}

