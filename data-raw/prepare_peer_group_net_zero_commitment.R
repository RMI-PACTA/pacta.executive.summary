#' Prepare data input for share of net zero committed companies in scorecard
#'
#' Prepare data input for share of net zero committed companies in scorecard
#' based on PACTA for investors output files.
#'
#' @param total_portfolio Data frame. Contains processed input of the PACTA for
#'   Investors calculation.
#' @param peer_group Character. Peer group of the analysed portfolio.
#' @param net_zero_targets Data frame. Contains information on which ISINs
#'   belong to companies that have committed to SBTI net zero targets.
#' @param peer_group_share_net_zero Data frame. Contains precalculated shares of
#'   companies in the peer group portfolios that have SBTI net zero commitments.
#'
#' @return data.frame
#' @export

prepare_peers_net_zero_commitment <- function(total_portfolio,
                                               net_zero_targets){
  company_number <- calculate_peers_company_number_sbti_commitments(total_portfolio,
                                                                    net_zero_targets)

  share <- calculate_peers_share_sbti_commitments(total_portfolio,
                                         net_zero_targets)
  peers_net_zero_commitment <- company_number %>%
    left_join(share, by = c("investor_name", "portfolio_name"))

  return(peers_net_zero_commitment)
}


calculate_peers_company_number_sbti_commitments <- function(total_portfolio,
                                                            net_zero_targets) {

  peer_group_company_number_net_zero <- total_portfolio_peers %>%
    dplyr::left_join(
      net_zero_targets,
      by = c("isin", "factset_entity_id")
    ) %>%
    dplyr::distinct(investor_name, portfolio_name, factset_entity_id, has_net_zero_commitment) %>%
    dplyr::group_by(.data$investor_name, .data$portfolio_name) %>%
    dplyr::summarise(
      company_share_net_zero = sum(.data$has_net_zero_commitment, na.rm = TRUE) / dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  return(peer_group_company_number_net_zero)
}

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
      exposure_share_net_zero = sum(.data$usd_value_net_zero_commitment, na.rm = TRUE) / sum(.data$value_usd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  return(peer_group_share_net_zero)
}
