#' Prepare data input for climate strategy metrics (initiatives) in the scorecard
#'
#' Prepare data input for climate strategy metrics (initiatives) in the
#' scorecard section based on COP survey results. The input data sets are
#' pre-calculated and accessed via the directories pertaining to the given
#' user_id.
#'
#' @param data Data frame that contains pre-calculated climate strategy info on
#'   participation in initiatives on the individual user level, based on the
#'   responses to the COP survey.
#' @param data_peers Data frame that contains pre-calculated climate strategy
#'   info on peer level participation in initiatives, based on the responses to
#'   the COP survey.
#' @param peer_group Character. Peer group of the analysed portfolio.
#'
#' @return data.frame
prep_climate_strategy_scorecard_initiatives <- function(data,
                                                        data_peers,
                                                        peer_group = c("pensionfund", "assetmanager", "bank", "insurance", "other")) {
  # match input arg
  peer_group <- match.arg(peer_group)

  # check peer groups match
  check_data_prep_climate_strategy_scorecard(
    data = data,
    peer_group = peer_group
  )

  # prepare data sets
  data <- data %>%
    dplyr::mutate(entity_type = "this_portfolio") %>%
    dplyr::mutate(yes = sum(climate_initiative, na.rm = TRUE)) %>%
    dplyr::filter(.data$peer_group == .env$peer_group) %>%
    dplyr::select(entity_type, peer_group, yes, name_climate_initiative)

  data_peers <- data_peers %>%
    dplyr::mutate(
      entity_type = "peers",
      name_climate_initiative = NA_character_
    ) %>%
    dplyr::filter(.data$peer_group == .env$peer_group) %>%
    dplyr::select(entity_type, peer_group, yes = peers_yes, name_climate_initiative)

  # combine data sets
  climate_strategy_scorecard_initiatives <- data %>%
    dplyr::bind_rows(data_peers)

  return(climate_strategy_scorecard_initiatives)
}

#' Prepare data input for climate strategy metrics (engagement) in the scorecard
#'
#' Prepare data input for climate strategy metrics (engagement) in the scorecard
#' section based on COP survey results. The input data sets are pre-calculated
#' and accessed via the directories pertaining to the given user_id.
#'
#' @param data Data frame that contains pre-calculated climate strategy info on
#'   client engagement on the individual user level, based on the
#'   responses to the COP survey.
#' @param data_peers Data frame that contains pre-calculated climate strategy
#'   info on peer level client engagement, based on the responses to
#'   the COP survey.
#' @param peer_group Character. Peer group of the analysed portfolio.
#'
#' @return data.frame
prep_climate_strategy_scorecard_engagement <- function(data,
                                                       data_peers,
                                                       peer_group = c("pensionfund", "assetmanager", "bank", "insurance", "other")) {
  # match input arg
  peer_group <- match.arg(peer_group)

  # check peer groups match
  check_data_prep_climate_strategy_scorecard(
    data = data,
    peer_group = peer_group
  )

  # prepare data sets
  data <- data %>%
    dplyr::mutate(entity_type = "this_portfolio") %>%
    dplyr::mutate(
      yes = dplyr::if_else(engagement == "yes", 1, 0),
      no = dplyr::if_else(engagement == "no", 1, 0),
      not_answered = dplyr::if_else(engagement == "not_answered", 1, 0)
    ) %>%
    dplyr::filter(.data$peer_group == .env$peer_group) %>%
    dplyr::select(entity_type, peer_group, asset_type, yes, no, not_answered)

  data_peers <- data_peers %>%
    dplyr::mutate(entity_type = "peers") %>%
    dplyr::filter(.data$peer_group == .env$peer_group) %>%
    dplyr::rename_with(~ gsub("peers_", "", .x, fixed = TRUE)) %>%
    dplyr::select(entity_type, peer_group, yes, no, not_answered)

  # combine data sets
  climate_strategy_scorecard_engagement <- data %>%
    dplyr::bind_rows(data_peers)

  return(climate_strategy_scorecard_engagement)
}

#' Prepare data input for climate strategy metrics (voting rights) in the scorecard
#'
#' Prepare data input for climate strategy metrics (voting rights) in the scorecard
#' section based on COP survey results. The input data sets are pre-calculated
#' and accessed via the directories pertaining to the given user_id.
#'
#' @param data Data frame that contains pre-calculated climate strategy info on
#'   use of voting rights on the individual user level, based on the
#'   responses to the COP survey.
#' @param data_peers Data frame that contains pre-calculated climate strategy
#'   info on peer level use of voting rights, based on the responses to
#'   the COP survey.
#' @param peer_group Character. Peer group of the analysed portfolio.
#'
#' @return data.frame
prep_climate_strategy_scorecard_voting <- function(data,
                                                   data_peers,
                                                   peer_group = c("pensionfund", "assetmanager", "bank", "insurance", "other")) {
  # match input arg
  peer_group <- match.arg(peer_group)

  # check peer groups match
  check_data_prep_climate_strategy_scorecard(
    data = data,
    peer_group = peer_group
  )

  # prepare data sets
  data <- data %>%
    dplyr::mutate(entity_type = "this_portfolio") %>%
    dplyr::mutate(
      yes = dplyr::if_else(voting_rights == "yes", 1, 0),
      no = dplyr::if_else(voting_rights == "no", 1, 0),
      not_answered = dplyr::if_else(voting_rights == "not_answered", 1, 0)
    ) %>%
    dplyr::filter(.data$peer_group == .env$peer_group) %>%
    dplyr::select(entity_type, peer_group, asset_type, yes, no, not_answered)

  data_peers <- data_peers %>%
    dplyr::mutate(entity_type = "peers") %>%
    dplyr::filter(.data$peer_group == .env$peer_group) %>%
    dplyr::rename_with(~ gsub("peers_", "", .x, fixed = TRUE)) %>%
    dplyr::select(entity_type, peer_group, yes, no, not_answered)

  # combine data sets
  climate_strategy_scorecard_voting <- data %>%
    dplyr::bind_rows(data_peers)

  return(climate_strategy_scorecard_voting)
}

check_data_prep_climate_strategy_scorecard <- function(data,
                                                       peer_group) {
  if (!peer_group == unique(data$peer_group)) {
    stop("Input value for peer_group does not match the value of the peer group
         found in the survey results of the user.")
  }
}
