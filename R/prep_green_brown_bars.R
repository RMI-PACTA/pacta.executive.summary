prep_green_brown_bars <- function(results_portfolio,
                                  scenario_selected) {
  # input data
  data <- results_portfolio

  min_year <- min(data$year, na.rm = TRUE)
  max_year <- max(data$year, na.rm = TRUE)

  # filter data
  data <- data %>%
    dplyr::filter(
      year %in% c(.env$min_year, .env$max_year),
      .data$scenario == .env$scenario_selected,
    )

  # map technologies and sectors
  # TODO: use input args to define groupings?
  # TODO: snake_case?
  data <- data %>%
    dplyr::mutate(
      ald_sector = dplyr::case_when(
        .data$ald_sector == "Coal" ~ "fossil_fuels",
        .data$ald_sector == "Oil&Gas" ~ "fossil_fuels",
        TRUE ~ .data$ald_sector
      )
    ) %>%
    dplyr::mutate(
      tech_type = dplyr::case_when(
        .data$ald_sector == "Power" & .data$technology == "NuclearCap" ~ "nuclear",
        .data$ald_sector %in% c("Aviation", "Cement", "Steel") ~ "other",
        TRUE ~ .data$green_or_brown
      )
    )

  # calculate technology and sector exposures
  data_out <- data %>%
    dplyr::select(
      .data$asset_class, .data$year, .data$tech_type,# .data$technology,
      .data$ald_sector, .data$plan_carsten
    ) %>%
    # dplyr::rename(perc_tech_exposure = .data$plan_carsten) %>%
    dplyr::group_by(
      .data$asset_class, .data$year, .data$ald_sector, .data$tech_type
    ) %>%
    dplyr::summarise(perc_tech_exposure = sum(.data$plan_carsten, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$asset_class, .data$year, .data$ald_sector) %>%
    dplyr::mutate(perc_sec_exposure = sum(.data$perc_tech_exposure, na.rm = TRUE)) %>%
    dplyr::ungroup()

  return(data_out)
}
