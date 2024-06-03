#' Prepare data input for plotting green brown bar chart
#'
#' Prepare data input for plotting green brown bar chart based on PACTA for
#' investors output files. These files must have been wrangled with
#' `prep_data_executive_summary()` before they can be passed to this function.
#'
#' @param results_portfolio Data frame that contains pre-wrangled portfolio
#'   level PACTA results from a PACTA for investors analysis.
#' @param scenario_selected Character. Must be a `scenario` featured in the
#'   `scenario_thresholds` data set. Default is "1.5C-Unif", which is the 1.5°C
#'   scenario from GECO2021.
#'
#' @return data.frame
#' @export
prep_green_brown_bars <- function(results_portfolio,
                                  scenario_selected = "NZE_2050") {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("green_brown_bars")
  } else {
    # check input
    check_data_prep_green_brown_bars(scenario_selected = scenario_selected)

    # input data
    data <- results_portfolio

    # infer start year
    start_year <- min(data$year, na.rm = TRUE)

    # filter data
    data <- data %>%
      dplyr::filter(
        .data$year == .env$start_year,
        .data$scenario == .env$scenario_selected
      )

    # map sectors to p4b style
    # map tech_type and fossil_fuels
    data <- data %>%
      # TODO: use input args to define groupings?
      map_sectors_and_tech_type()

    # calculate tech_type and sector exposures
    data_out <- data %>%
      calculate_exposures() %>%
      dplyr::select(
        c("asset_class", "year", "tech_type", "sector", "perc_tech_exposure",
          "perc_sec_exposure")
      )
  }
  data_out
}

check_data_prep_green_brown_bars <- function(scenario_selected) {
  if (!scenario_selected %in% unique(get("scenario_thresholds")$scenario)) {
    stop("Argument scenario_selected does not hold an accepted value.")
  }
  if (length(scenario_selected) != 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
}

map_sectors_and_tech_type <- function(data) {
  data <- data %>%
    dplyr::inner_join(
      get("p4i_p4b_sector_technology_mapper"),
      by = c("ald_sector" = "sector_p4i", "technology" = "technology_p4i")
    ) %>%
    dplyr::mutate(
      ald_sector = .data$sector_p4b,
      technology = .data$technology_p4b
    ) %>%
    dplyr::select(-c("sector_p4b", "technology_p4b")) %>%
    dplyr::mutate(
      ald_sector = dplyr::case_when(
        .data$ald_sector == "coal" ~ "fossil_fuels",
        .data$ald_sector == "oil_and_gas" ~ "fossil_fuels",
        TRUE ~ .data$ald_sector
      )
    ) %>%
    dplyr::mutate(
      tech_type = dplyr::case_when(
        .data$ald_sector == "power" & .data$technology == "nuclearcap" ~ "nuclear",
        .data$ald_sector %in% c("aviation", "cement", "steel") ~ "other",
        TRUE ~ .data$green_or_brown
      )
    )

  return(data)
}

calculate_exposures <- function(data) {
  data <- data %>%
    dplyr::select(
      c("investor_name", "portfolio_name", "entity_name", "entity_type", "entity",
        "asset_class", "year", "tech_type", "ald_sector", "plan_carsten")
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name,.data$entity_name,
      .data$entity_type, .data$entity, .data$asset_class, .data$year,
      .data$tech_type, .data$ald_sector
    ) %>%
    dplyr::summarise(perc_tech_exposure = sum(.data$plan_carsten, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$entity_name,
      .data$entity_type, .data$entity, .data$asset_class, .data$year,
      .data$ald_sector
    ) %>%
    dplyr::mutate(perc_sec_exposure = sum(.data$perc_tech_exposure, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(sector = "ald_sector") %>%
    dplyr::arrange(
      .data$investor_name, .data$portfolio_name, .data$sector, .data$asset_class,
      .data$tech_type
    )

  return(data)
}
