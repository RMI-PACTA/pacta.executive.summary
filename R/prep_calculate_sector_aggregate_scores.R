prep_calculate_sector_aggregate_scores <- function(data) {
  # create helper variables for the weighting factors
  # account for special handling of CoalCap, RenewablesCap, ICE and Electric
  data <- data %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class, .data$entity,
      .data$scenario_source, .data$scenario, .data$allocation,
      .data$equity_market, .data$scenario_geography, .data$ald_sector,
      .data$technology
    ) %>%
    dplyr::mutate(
      tech_allocation_weight = dplyr::if_else(
        # TODO: add an input that defines these technologies
        .data$technology %in% c("CoalCap", "RenewablesCap", "ICE", "Electric") &
          .data$plan_alloc_wt_tech_prod_t5 > .data$scen_alloc_wt_tech_prod_t5,
        .data$plan_alloc_wt_tech_prod_t5,
        .data$scen_alloc_wt_tech_prod_t5
      )
    ) %>%
    dplyr::mutate(
      scenario_change = abs(.data$scen_alloc_wt_tech_prod_t5 - .data$scen_alloc_wt_tech_prod_t0),
      production_change = abs(.data$plan_alloc_wt_tech_prod_t5 - .data$scen_alloc_wt_tech_prod_t0)
    ) %>%
    dplyr::mutate(
      scenario_change = dplyr::if_else(
        .data$technology %in% c("CoalCap", "RenewablesCap", "ICE", "Electric") &
          .data$production_change > .data$scenario_change,
        .data$production_change,
        .data$scenario_change
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$production_change)

  # calculate sector score & sector exposure
  data <- data %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class, .data$entity,
      .data$scenario_source, .data$scenario, .data$allocation,
      .data$equity_market, .data$scenario_geography, .data$ald_sector
    ) %>%
    dplyr::summarise(
      score = stats::weighted.mean(
         x = .data$tech_trajectory_alignment,
         w = .data$tech_allocation_weight * .data$scenario_change
       ),
      sector_exposure = sum(.data$exposure_t0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(scope = "sector")

  return(data)
}
