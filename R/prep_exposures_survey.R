#' Prepare data input for plotting exposure to fossil fuels in survey section
#'
#' Prepare data input for plotting exposure to fossil fuels in survey section
#' based on PACTA for investors output files. These files must have been
#' wrangled with `prep_data_executive_summary()` before they can be passed to
#' this function.
#'
#' @param results_portfolio Data frame that contains pre-wrangled portfolio
#'   level PACTA results from a PACTA for investors analysis.
#' @param peers_results_aggregated Data frame that contains pre-wrangled
#'   aggregate peer group level PACTA results from a PACTA for investors
#'   analysis.
#' @param technology Character. Must be of length 1 and either `coal` or
#'   `oil` or `gas`.
#' @param asset_class Character. Must be of length 1 and either `equity` or
#'   `bonds`.
#'
#' @return data.frame
#' @export
prep_exposures_survey <- function(results_portfolio,
                                  peers_results_aggregated,
                                  technology = c("coal", "oil", "gas"),
                                  asset_class = c("equity", "bonds")) {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("exposures_survey") %>%
      filter(
        .data$asset_class == .env$asset_class,
        .data$technology == .env$technology
      )
  } else {
    # validate inputs
    sector <- match.arg(technology)
    asset_class <- match.arg(asset_class)

    check_data_prep_exposures_survey(
      asset_class = asset_class,
      technology = technology
    )

    # available portfolio asset classes
    available_asset_classes <- unique(results_portfolio$asset_class)

    # infer start year
    start_year <- min(results_portfolio$year, na.rm = TRUE)

    # pick scenario for filtering (no impact on current exposure)
    scenario_filter <- results_portfolio %>%
      pull(.data$scenario) %>%
      head(1)

    # combine input data
    data <- results_portfolio %>%
      dplyr::bind_rows(peers_results_aggregated)

    # keep only asset_class available in portfolio
    data <- data %>%
      dplyr::filter(.data$asset_class %in% available_asset_classes)

    # calculate current exposures
    data <- data %>%
      dplyr::filter(
        .data$year == .env$start_year,
        .data$scenario == .env$scenario_filter
      )

    # wrangle to expected format
    data <- data %>%
      wrangle_data_exposures_survey()

    data_out <- data %>%
      dplyr::filter(
        .data$asset_class == .env$asset_class,
        .data$technology == .env$technology
      )
  }
  data_out
}

check_data_prep_exposures_survey <- function(asset_class,
                                             technology) {
  if (length(asset_class) != 1) {
    stop("Argument asset_class must be of length 1. Please check your input.")
  }
  if (length(technology) != 1) {
    stop("Argument sector must be of length 1. Please check your input.")
  }
}

wrangle_data_exposures_survey <- function(data) {
  data <- data %>%
    dplyr::inner_join(
      get("p4i_p4b_sector_technology_mapper"),
      by = c("ald_sector" = "sector_p4i", "technology" = "technology_p4i")
    ) %>%
    dplyr::mutate(
      ald_sector = .data$sector_p4b,
      technology = .data$technology_p4b,
      entity = replace(.data$entity, .data$entity == "this_portfolio", "portfolio")
    ) %>%
    dplyr::select(c("asset_class", "entity", "technology", "plan_carsten")) %>%
    dplyr::rename(
      exposure_perc_aum = "plan_carsten"
    ) %>%
    dplyr::group_by(.data$asset_class, .data$entity, .data$technology) %>%
    dplyr::summarise(
      exposure_perc_aum = sum(.data$exposure_perc_aum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()
}
