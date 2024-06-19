library(tidyverse)

companies_sbti <- readxl::read_xlsx(
  file.path("data-raw", "companies-taking-action.xlsx"),
  sheet = "Worksheet"
)

# read financial data file
fin_data <- readr::read_rds(
  file.path("../pacta-data", "2023Q4", "financial_data.rds")
)

prep_fin_data_net_zero_targets <- function(data,
                                           financial_data) {
  # keep only companies with relevant targets
  companies_sbti_net_zero <- data %>%
    dplyr::filter(
      `Near term - Target Status` == "Targets Set",
      `Net-Zero Committed` == "Yes",
      `Near term - Target Classification` == "1.5°C"
    ) %>%
    dplyr::rename(isin = ISIN)

  # for all companies that have no factset id, merge the factset id from the
  # financial data, based on isin
  # remove rows with remaining missing values for factset id
  companies_sbti_net_zero_completed <- companies_sbti_net_zero %>%
    dplyr::left_join(
      financial_data,
      by = "isin"
    ) %>%
    dplyr::mutate(Factset_id = ifelse(is.na(factset_entity_id), factset_entity_id, factset_entity_id)) %>%
    dplyr::filter(!is.na(Factset_id))

  # keep all fin_data entries that have a factset_entity_id belonging to companies
  # that have net zero targets (this will include isins that may have been missed)
  fin_data_net_zero_targets <- financial_data %>%
    dplyr::mutate(
      has_net_zero_commitment = ifelse(factset_entity_id %in% companies_sbti_net_zero_completed$Factset_id, TRUE, FALSE)
    )%>%
    dplyr::select(dplyr::all_of(c("isin", "asset_type", "factset_entity_id", "has_net_zero_commitment")))

  return(fin_data_net_zero_targets)
}

fin_data_net_zero_targets <- prep_fin_data_net_zero_targets(
  data = companies_sbti,
  financial_data = fin_data
)

usethis::use_data(fin_data_net_zero_targets, overwrite = TRUE)
