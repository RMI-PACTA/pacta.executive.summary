#' Prepare data input for plotting aggregate climate scores in the scorecard
#' section
#'
#' Prepare data input for plotting aggregate climate scores in the scorecard
#' section based on PACTA for investors output files. These files must have been
#' wrangled with `prep_data_executive_summary()` before they can be passed to
#' this function. This simply calls `prep_scores()` and filters the appropriate
#' values.
#'
#' @param results_portfolio Data frame that contains pre-wrangled portfolio
#'   level PACTA results from a PACTA for investors analysis.
#' @param scenario_source Character. Must be a
#'   `scenario_source` featured in the `scenario_thresholds` data set.
#'
#' @return data.frame
#' @export
prep_scores_scorecard <- function(results_portfolio,
                                  scenario_source = "GECO2021") {
  if (is.null(results_portfolio)) {
    data_out <- use_toy_data("scores") %>% filter(
      .data$scope == "portfolio",
      .data$entity == "this_portfolio"
    )
  } else {
    empty_peers_results_aggregated <- tibble::tibble()

    # input checks are automatically carried out when calling prep_scores()
    data_equity <- results_portfolio %>%
      prep_scores(
        peers_results_aggregated = empty_peers_results_aggregated,
        asset_class = "equity",
        scenario_source = scenario_source
      )
    data_bonds <- results_portfolio %>%
      prep_scores(
        peers_results_aggregated = empty_peers_results_aggregated,
        asset_class = "bonds",
        scenario_source = scenario_source
      )

    data_out <- data_equity %>%
      rbind(data_bonds) %>%
      dplyr::filter(
        .data$scope == "portfolio",
        .data$entity == "this_portfolio"
      )
  }
  data_out
}
