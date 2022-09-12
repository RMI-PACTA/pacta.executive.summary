prep_calculate_portfolio_aggregate_scores <- function(data,
                                                      remaining_carbon_budgets) {
  data <- data %>%
    dplyr::inner_join(
      remaining_carbon_budgets,
      by = c("scenario_source", "ald_sector")
    ) %>%
    dplyr::group_by(
      .data$investor_name, .data$portfolio_name, .data$asset_class,
      .data$entity, .data$scenario_source, .data$scenario, .data$allocation,
      .data$equity_market, .data$scenario_geography
    ) %>%
    dplyr::summarise(
      score = stats::weighted.mean(
        x = .data$score,
        w = .data$remaining_carbon_budget * .data$sector_exposure
      ),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      scope = "portfolio",
      ald_sector = NA_character_
    )

  return(data)
}
