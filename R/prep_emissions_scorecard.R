#' Title
#'
#' @param emissions_data Some arg
#' @param audit_data Some arg
#' @param currency_exchange_value Some arg
#'
#' @return Some output data
#' @export
prep_emissions_scorecard <- function(emissions_data = NULL,
                                     audit_data,
                                     currency_exchange_value) {
  if (is.null(emissions_data)) {
    data_out <- use_toy_data("emissions_scorecard")
  }

  # available portfolio asset classes
  available_asset_classes <- audit_data %>%
    dplyr::filter(
      .data$entity == "portfolio",
      .data$financial_sector %in% c("Automotive", "Coal", "Oil&Gas", "Power", "Aviation", "Cement", "Steel")
    ) %>%
    dplyr::mutate(asset_class = tolower(.data$asset_type)) %>%
    dplyr::pull("asset_class") %>%
    unique()

  # prepare audit data
  portfolio_value_currency_asset_type <- audit_data %>%
    dplyr::filter(.data$asset_type %in% c("Equity", "Bonds")) %>%
    dplyr::group_by(.data$asset_type, .data$entity) %>%
    dplyr::summarise(
      value_curr_mio = sum(.data$value_usd / .env$currency_exchange_value, na.rm = TRUE) / 1000000,
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(asset_class = tolower(.data$asset_type)) %>%
    dplyr::select(-"asset_type")

  # prepare emissions data
  emissions_asset_entity <- emissions_data %>%
    dplyr::group_by(.data$asset_type, .data$entity) %>%
    dplyr::mutate(emissions_asset = sum(.data$weighted_sector_emissions, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$asset_type, .data$entity, .data$emissions_asset) %>%
    dplyr::summarise(
      emissions = sum(.data$weighted_sector_emissions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(c("asset_type", "entity", "emissions")) %>%
    dplyr::rename(asset_class = "asset_type") %>%
    dplyr::mutate(asset_class = tolower(.data$asset_class))

  # join and filter
  data_out <- emissions_asset_entity %>%
    dplyr::inner_join(
      portfolio_value_currency_asset_type,
      by = c("asset_class", "entity")
    ) %>%
    dplyr::filter(.data$asset_class %in% available_asset_classes) %>%
    dplyr::mutate(emissions = round(.data$emissions / .data$value_curr_mio, 1)) %>%
    dplyr::select(c("asset_class", "entity", "emissions"))

  return(data_out)
}
