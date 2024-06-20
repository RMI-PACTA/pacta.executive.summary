load(file.path("../pacta.executive.summary", "data", "fin_data_net_zero_targets.rda"))

path_total_portfolio_peers <- file.path("path", "to", "peers", "30_Processed_inputs", "total_portfolio.rds")

total_portfolio_peers <- readRDS(path_total_portfolio_peers)

calculate_peers_share_sbti_commitments <- function(total_portfolio,
                                             net_zero_targets) {

  peer_group_share_net_zero <- total_portfolio %>%
    dplyr::left_join(
      net_zero_targets,
      by = c("isin", "factset_entity_id", "asset_type")
    )  %>%
    dplyr::filter(asset_type %in% c("Bonds", "Equity")) %>%
    mutate(
      usd_value_net_zero_commitment = ifelse(.data$has_net_zero_commitment,
                                             .data$value_usd,
                                             0
      )
    )  %>%
    dplyr::group_by(.data$investor_name, .data$portfolio_name) %>%
    dplyr::summarise(
      share_net_zero = sum(.data$usd_value_net_zero_commitment, na.rm = TRUE) / sum(.data$value_usd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  return(peer_group_share_net_zero)
}

peer_group_share_net_zero <- calculate_peers_share_sbti_commitments(
  total_portfolio = total_portfolio_peers,
  net_zero_targets = fin_data_net_zero_targets
)

usethis::use_data(peer_group_share_net_zero, overwrite = TRUE)
