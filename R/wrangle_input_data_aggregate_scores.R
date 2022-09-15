wrangle_input_data_aggregate_scores <- function(data,
                                                scenarios) {
  # filter data
  data <- data %>%
    dplyr::filter(.data$scenario %in% .env$scenarios)

  # map sectors to p4b style
  data <- data %>%
    dplyr::inner_join(
      get("p4i_p4b_sector_technology_mapper"),
      by = c("ald_sector" = "sector_p4i", "technology" = "technology_p4i")
    ) %>%
    dplyr::mutate(
      ald_sector = .data$sector_p4b,
      technology = .data$technology_p4b
    ) %>%
    dplyr::select(-c(.data$sector_p4b, .data$technology_p4b))

  # Oil & Gas are treated as separate sectors in the aggregate score.
  data <- data %>%
    dplyr::mutate(
      ald_sector = dplyr::case_when(
        .data$technology == "oil" ~ "oil",
        .data$technology == "gas" ~ "gas",
        TRUE ~ .data$ald_sector
      )
    )

  return(data)
}
