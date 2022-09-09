calculate_aggregate_scores_with_scenarios <- function(data,
                                                      scenario_thresholds) {
  # add threshold scenarios
  data <- data %>%
    dplyr::select(
      .data$asset_class, .data$scope, .data$entity, .data$ald_sector,
      .data$scenario_source, .data$scenario, .data$score
    ) %>%
    dplyr::inner_join(
      scenario_thresholds,
      by = c("scenario_source", "scenario")
    ) %>%
    dplyr::select(-c(.data$scenario, .data$scenario_source))

  data <- data %>%
    tidyr::pivot_wider(
      id_cols = c(
        .data$asset_class, .data$scope, .data$entity, .data$ald_sector,

      ),
      names_from = .data$threshold,
      values_from = .data$score
    ) %>%
    dplyr::rename(sector = .data$ald_sector)

  # calculate grades based on scores and scenario thresholds
  data <- data %>%
    dplyr::mutate(
      score = dplyr::case_when(
        .data$`1.5C` > 0.15 ~ "A+",
        .data$`1.5C` >= 0 ~ "A",
        .data$`1.5C` < 0 & .data$`1.5C` >= -0.15 & .data$`2.0C` >= 0 ~ "B",
        .data$`2.0C` >= 0 ~ "C",
        .data$ref >= 0 ~ "D",
        .data$ref < 0 ~ "E",
        TRUE ~ NA_character_,
      )
    )

  data <- data %>%
    dplyr::select(
      .data$asset_class, .data$scope, .data$entity, .data$sector, .data$score
    ) %>%
    dplyr::arrange(.data$asset_class, .data$scope, .data$sector, .data$entity)

  return(data)
}
