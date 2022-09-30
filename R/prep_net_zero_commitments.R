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
prep_net_zero_commitments <- function(total_portfolio,
                                      peer_group = c("pensionfund", "assetmanager", "bank", "insurance", "other"),
                                      net_zero_targets,
                                      peer_group_share_net_zero) {
  # TODO: add allowed values for peer_group arg
  peer_group <- match.arg(peer_group)

  net_zero_targets <- net_zero_targets %>%
    dplyr::mutate(has_net_zero_commitment = TRUE)

  portfolio_share_net_zero <- total_portfolio %>%
    dplyr::mutate(
      investor_name = "this_portfolio",
      portfolio_name = "this_portfolio"
    ) %>%
    dplyr::left_join(
      net_zero_targets,
      by = c("isin", "factset_entity_id")
    ) %>%
    dplyr::distinct(investor_name, portfolio_name, factset_entity_id, has_net_zero_commitment) %>%
    dplyr::group_by(.data$investor_name, .data$portfolio_name) %>%
    dplyr::summarise(
      share_net_zero = sum(.data$has_net_zero_commitment, na.rm = TRUE) / dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  peer_group_share_net_zero <- peer_group_share_net_zero %>%
    dplyr::filter(.data$investor_name == .env$peer_group)

  share_net_zero <- portfolio_share_net_zero %>%
    dplyr::bind_rows(peer_group_share_net_zero)

  return(share_net_zero)
}
