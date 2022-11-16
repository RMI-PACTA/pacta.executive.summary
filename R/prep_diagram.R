#' Title
#'
#' @param audit_data Some arg
#' @param emissions_data Some arg
#'
#' @return
#' @export
prep_diagram <- function(audit_data = NULL, emissions_data = NULL) {
  if (is.null(audit_data) | is.null(emissions_data)) {
    data_out <- use_toy_data("diagram")
  }

  audit_data <- audit_data %>%
    dplyr::filter(.data$entity == "portfolio") %>%
    dplyr::mutate(exposure_portfolio = sum(.data$value_usd, na.rm = TRUE)) %>%
    dplyr::group_by(.data$asset_type) %>%
    dplyr::mutate(
      exposure_asset_class = sum(.data$value_usd, na.rm = TRUE),
      exposure_asset_class_perc = .data$exposure_asset_class / .data$exposure_portfolio
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      covered_pacta = dplyr::if_else(
        .data$financial_sector %in% c("Automotive", "Coal", "Oil&Gas", "Power", "Aviation", "Cement", "Steel"),
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::group_by(
      .data$asset_type, .data$exposure_portfolio, .data$covered_pacta,
      .data$exposure_asset_class, .data$exposure_asset_class_perc
    ) %>%
    dplyr::summarise(
      exposure_pacta = sum(.data$value_usd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      exposure_pacta_perc_asset_class_exposure = .data$exposure_pacta / .data$exposure_asset_class
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$covered_pacta) %>%
    dplyr::select(
      c("exposure_portfolio", "asset_type", "exposure_asset_class",
        "exposure_asset_class_perc", "exposure_pacta", "exposure_pacta_perc_asset_class_exposure")
    )

  emissions_data <- emissions_data %>%
    dplyr::filter(.data$entity == "portfolio") %>%
    dplyr::group_by(.data$asset_type) %>%
    # TODO: check if this what we need or if it needs to be divivded by the asset value covered
    dplyr::mutate(emissions_asset = sum(.data$weighted_sector_emissions, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      covered_pacta = dplyr::if_else(
        .data$sector %in% c("Automotive", "Coal", "Oil&Gas", "Power", "Aviation", "Cement", "Steel"),
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::group_by(.data$asset_type, .data$covered_pacta, .data$emissions_asset) %>%
    dplyr::summarise(
      emissions_pacta = sum(.data$weighted_sector_emissions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(emissions_pacta_perc = .data$emissions_pacta / .data$emissions_asset) %>%
    dplyr::filter(.data$covered_pacta) %>%
    dplyr::select(c("asset_type", "emissions_pacta_perc", "emissions_pacta"))

  data_out <- audit_data %>%
    dplyr::inner_join(emissions_data, by = "asset_type") %>%
    dplyr::rename(asset_class = "asset_type") %>%
    dplyr::mutate(asset_class = tolower(.data$asset_class))

  missing_asset_types <- setdiff(c("equity", "bonds"), unique(data_out$asset_class))

  if (any(c("equity", "bonds") %in% missing_asset_types)) {
    missing_assets <- tibble::tibble(
      exposure_portfolio = unique(data_out$exposure_portfolio),
      asset_class = missing_asset_types,
      exposure_asset_class = 0,
      exposure_asset_class_perc = 0,
      exposure_pacta = 0,
      exposure_pacta_perc_asset_class_exposure = 0,
      emissions_pacta_perc = 0,
      emissions_pacta = 0
    )

    data_out <- rbind(data_out, missing_assets)
  }

  return(data_out)
}
