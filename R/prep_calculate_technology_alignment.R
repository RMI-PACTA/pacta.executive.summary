prep_calculate_technology_alignment <- function(data,
                                                green_techs,
                                                start_year,
                                                time_horizon) {
  data <- data %>%
    dplyr::mutate(
      green_or_brown = dplyr::if_else(
        # TODO: be more explicit here?
        .data$technology %in% .env$green_techs, "green", "brown"
      )
    ) %>%
    dplyr::select(
      .data$investor_name, .data$portfolio_name, .data$asset_class, .data$entity,
      .data$scenario_source, .data$scenario, .data$allocation,
      .data$equity_market, .data$scenario_geography, .data$ald_sector,
      .data$technology, .data$year, .data$plan_alloc_wt_tech_prod,
      .data$plan_carsten, .data$scen_alloc_wt_tech_prod, .data$green_or_brown
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class, .data$entity,
      .data$scenario_source, .data$scenario, .data$allocation,
      .data$equity_market, .data$scenario_geography, .data$ald_sector,
      .data$technology
    ) %>%
    dplyr::mutate(
      scen_alloc_wt_tech_prod_t0 = dplyr::first(.data$scen_alloc_wt_tech_prod),
      scen_alloc_wt_tech_prod_t5 = dplyr::last(.data$scen_alloc_wt_tech_prod),
      plan_alloc_wt_tech_prod_t5 = dplyr::last(.data$plan_alloc_wt_tech_prod),
      exposure_t0 = dplyr::first(.data$plan_carsten)
    ) %>%
    dplyr::select(-.data$scen_alloc_wt_tech_prod) %>%
    dplyr::filter(.data$year == .env$start_year + .env$time_horizon) %>%
    dplyr::mutate(
      tech_trajectory_alignment = dplyr::if_else(
        .data$green_or_brown == "green",
        (.data$plan_alloc_wt_tech_prod - .data$scen_alloc_wt_tech_prod_t5) /
          .data$scen_alloc_wt_tech_prod_t5,
        (.data$scen_alloc_wt_tech_prod_t5 - .data$plan_alloc_wt_tech_prod) /
          .data$scen_alloc_wt_tech_prod_t5
      )
    ) %>%
    dplyr::ungroup()

  return(data)
}
