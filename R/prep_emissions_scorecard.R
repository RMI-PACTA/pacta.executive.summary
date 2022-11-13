prep_emissions_scorecard <- function(emissions_data = NULL) {
  if (is.null(emissions_data)) {
    data_out <- use_toy_data("emissions_scorecard")
  }

  data_out <- emissions_data %>%
    dplyr::group_by(.data$asset_type, .data$entity) %>%
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
    dplyr::group_by(.data$asset_type, .data$entity, .data$covered_pacta, .data$emissions_asset) %>%
    dplyr::summarise(
      emissions = sum(.data$weighted_sector_emissions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$covered_pacta) %>%
    dplyr::select(c("asset_type", "entity", "emissions")) %>%
    dplyr::rename(asset_class = "asset_type") %>%
    dplyr::mutate(asset_class = tolower(.data$asset_class))

  return(data_out)
}
