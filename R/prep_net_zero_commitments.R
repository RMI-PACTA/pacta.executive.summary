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
#' @param peers_net_zero_commitment DataFrame. Contains informations on results at peer group level to be compared with portfolio
#'
#' @return data.frame
#' @export
prep_net_zero_commitments <- function(total_portfolio,
                                      peer_group = c("pensionfund", "assetmanager", "bank", "insurance", "other"),
                                      net_zero_targets,
                                      peers_net_zero_commitment
                                      ) {
  # match input arg
  peer_group <- match.arg(peer_group)

  # calculate portfolio level share of sbti commitments
  portfolio_number_company_net_zero <- prep_portfolio_sbti_commitments(
    total_portfolio = total_portfolio,
    net_zero_targets = net_zero_targets
  )

  portfolio_portfolio_share_net_zero <- prep_portfolio_sbti_compliant_commitments(
    total_portfolio = total_portfolio,
    net_zero_targets = net_zero_targets
  )

  # get peers sbti commitment for appropriate peer group
  peers_net_zero_commitment <- peers_net_zero_commitment %>%
    dplyr::filter(.data$investor_name == .env$peer_group)  %>%
    dplyr::mutate(investor_name = "peergroup",
           portfolio_name = "peergroup")

  # combine portfolio and peer level results
  data_out <- portfolio_number_company_net_zero %>%
    dplyr::left_join(portfolio_portfolio_share_net_zero, by = c("investor_name", "portfolio_name")) %>%
    dplyr::bind_rows(peers_net_zero_commitment) %>%
    tidyr::pivot_longer(cols = c("company_share_net_zero", "exposure_share_net_zero")) %>%
    dplyr::select(-"investor_name") %>%
    tidyr::pivot_wider(
      names_from ="portfolio_name"
    )

  return(data_out)
}

prep_portfolio_sbti_commitments <- function(total_portfolio,
                                            net_zero_targets) {

  portfolio_sbti_commitments <- total_portfolio %>%
    dplyr::mutate(
      investor_name = "this_portfolio",
      portfolio_name = "this_portfolio"
    ) %>%
    dplyr::left_join(
      net_zero_targets,
      by = c("isin", "factset_entity_id")
    ) %>%
    dplyr::distinct(.data$investor_name, .data$portfolio_name, .data$factset_entity_id, .data$has_net_zero_commitment) %>%
    dplyr::group_by(.data$investor_name, .data$portfolio_name) %>%
    dplyr::summarise(
      company_share_net_zero = sum(.data$has_net_zero_commitment, na.rm = TRUE) / dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()

  return(portfolio_sbti_commitments)
}

prep_portfolio_sbti_compliant_commitments <- function(total_portfolio,
                                                      net_zero_targets) {
  portfolio_sbti_compliant_commitments <- total_portfolio %>%
    dplyr::mutate(
      investor_name = "this_portfolio",
      portfolio_name = "this_portfolio"
    ) %>%
    dplyr::left_join(
      net_zero_targets,
      by = c("isin", "factset_entity_id", "asset_type")
    )  %>%
    dplyr::filter(.data$asset_type %in% c("Bonds", "Equity")) %>%
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
}
