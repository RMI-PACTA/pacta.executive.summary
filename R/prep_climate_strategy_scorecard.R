#' Prepare data input for climate strategy metrics in the scorecard section
#'
#' Prepare data input for climate strategy metrics in the scorecard section
#' based on COP survey results. The input data sets are pre-calculated and
#' accessed via the directories pertaining to the given user_id.
#'
#' @param user_climate_strategy_scorecard Data frame that contains
#'   pre-calculated climate strategy metrics on the user level, based on the
#'   responses to the COP survey.
#' @param peers_climate_strategy_scorecard Data frame that contains
#'   pre-calculated climate strategy metrics on the peer group level, based on
#'   the responses to the COP survey.
#' @param peer_group Character. Peer group of the analysed portfolio.
#'
#' @return data.frame
prep_climate_strategy_scorecard <- function (user_climate_strategy_scorecard,
                                             peers_climate_strategy_scorecard,
                                             peer_group = c("pensionfund", "assetmanager", "bank", "insurance", "other")) {
  # match input arg
  peer_group <- match.arg(peer_group)

  # check peer groups match
  check_data_prep_climate_strategy_scorecard(
    user_climate_strategy_scorecard = user_climate_strategy_scorecard,
    peer_group = peer_group
  )

  # prepare data sets
  user_climate_strategy_scorecard <- user_climate_strategy_scorecard %>%
    dplyr::mutate(entity_type = "this_portfolio") %>%
    dplyr::filter(.data$peer_group == .env$peer_group) %>%
    dplyr::select(entity_type, peer_group, name_climate_initiative, climate_initiative, engagement, voting)

  peers_climate_strategy_scorecard <- peers_climate_strategy_scorecard %>%
    dplyr::mutate(
      entity_type = "peers",
      name_climate_initiative = NA_character_
    ) %>%
    dplyr::filter(.data$peer_group == .env$peer_group) %>%
    dplyr::select(entity_type, peer_group, name_climate_initiative, climate_initiative, engagement, voting)

  # combine data sets
  climate_strategy_scorecard <- user_climate_strategy_scorecard %>%
    dplyr::bind_rows(peers_climate_strategy_scorecard)

  return(climate_strategy_scorecard)
}

check_data_prep_climate_strategy_scorecard <- function(user_climate_strategy_scorecard,
                                                       peer_group) {
  if (!peer_group == unique(user_climate_strategy_scorecard$peer_group)) {
    stop("Input value for peer_group does not match the value of the peer group
         found in the survey results of the user.")
  }
}

