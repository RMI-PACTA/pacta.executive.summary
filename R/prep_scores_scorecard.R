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

#' Prepare share of portfolio emissions covered by aggregate score analysis
#'
#' @param emissions_data Data frame that contains pre-wrangled emissions data
#' @param log_dir Path to log file
#'
#' @return numeric
#' @export
prep_scores_emissions_scorecard <- function(emissions_data,
                                            log_dir) {
  if (is.null(emissions)) {
    data_out <- NULL
    return(data_out)
  } else {
    tryCatch(
      {
        data_out <- emissions_data %>%
          dplyr::filter(.data$entity == "portfolio") %>%
          dplyr::mutate(
            covered_aggregate_score = dplyr::if_else(
              # sectors currently included in aggregate score calculation
              .data$sector %in% c("Automotive", "Coal", "Oil&Gas", "Power", "Aviation", "Steel"),
              TRUE,
              FALSE
            )
          ) %>%
          dplyr::group_by(.data$covered_aggregate_score) %>%
          dplyr::summarise(
            emissions_pacta = sum(.data$weighted_sector_emissions, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            emissions_portfolio = sum(.data$emissions_pacta, na.rm = TRUE)
          ) %>%
          dplyr::mutate(
            emissions_pacta_percent = .data$emissions_pacta / .data$emissions_portfolio
          ) %>%
          dplyr::filter(.data$covered_aggregate_score) %>%
          dplyr::pull("emissions_pacta_percent")

        stopifnot(length(data_out) == 1L)

        return(data_out)
      },
      error = function(e) {
        write_log(
          "ES: There was an error in prep_scores_emissions_scorecard().\nReturning NULL.\n",
          file_path = log_dir
        )
        NULL
      }
    )
  }
}

#' Prepare share of portfolio emissions covered by aggregate score analysis
#'
#' @param audit_data Data frame that contains pre-wrangled audit data
#' @param currency_exchange_value Numeric vector with exchange rate
#' @param total_portfolio_value_curr Numeric vector with total portfolio value
#'   in target currency
#' @param log_dir Path to log file
#'
#' @return numeric
#' @export
prep_scores_exposure_scorecard <- function(audit_data,
                                           currency_exchange_value,
                                           total_portfolio_value_curr,
                                           log_dir) {
  if (is.null(emissions) || is.null(currency_exchange_value) || is.null(total_portfolio_value_curr)) {
    data_out <- NULL
    return(data_out)
  } else {
    tryCatch(
      {
        value_covered <- audit_data %>%
          dplyr::filter(.data$entity == "portfolio") %>%
          dplyr::filter(.data$asset_type %in% c("Bonds", "Equity")) %>%
          # sectors currently included in aggregate score calculation
          dplyr::filter(
            .data$financial_sector %in% c("Automotive", "Coal", "Oil&Gas", "Power", "Aviation", "Steel")
          ) %>%
          dplyr::mutate(value_curr = .data$value_usd / .env$currency_exchange_value) %>%
          dplyr::pull("value_curr") %>%
          sum(na.rm = TRUE)

        data_out <- value_covered / total_portfolio_value_curr

        stopifnot(length(data_out) == 1L)

        return(data_out)
      },
      error = function(e) {
        write_log(
          "ES: There was an error in prep_scores_exposure_scorecard().\nReturning NULL.\n",
          file_path = log_dir
        )
        NULL
      }
    )
  }
}
