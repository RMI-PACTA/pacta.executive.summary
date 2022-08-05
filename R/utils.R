use_toy_data <- function(plot_name) {
  data_out <- get(glue::glue("toy_data_", plot_name))
  rlang::warn("No input data provided. Using toy data as output.")
  data_out
}

abort_if_has_zero_rows <- function(data, env) {
  .data <- deparse_1(substitute(data, env = env))
  if (nrow(data) == 0L) {
    abort(c(
      glue("`{.data}` must have some rows."),
      x = glue("`{.data}` has zero rows.")
    ))
  }

  invisible(data)
}

#' Check if a named object contains expected names
#'
#' Based on fgeo.tool::abort_if_missing_names()
#'
#' @param x A named object.
#' @param expected_names String; expected names of `x`.
#'
#' @return Invisible `x`, or an error with informative message.
#'
#' @examples
#' x <- c(a = 1)
#' abort_if_missing_names(x, "a")
#' try(abort_if_missing_names(x, "bad"))
#' @noRd
abort_if_missing_names <- function(data, expected_names) {
  stopifnot(rlang::is_named(data))
  stopifnot(is.character(expected_names))

  if (!all(unique(expected_names) %in% names(data))) {
    missing_names <- sort(setdiff(expected_names, names(data)))
    abort(c(
      "`data` must have all the expected names.",
      x = glue("Missing names: {toString(missing_names)}.")
    ),
    class = "missing_names"
    )
  }

  invisible(data)
}

# Backport `base::deparse1()` to R < 4.0.0
deparse_1 <- function(expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}

abort_if_invalid_values <- function(data, column, valid) {
  .column <- deparse_1(substitute(column))

  if (!all(data[[column]] %in% valid)) {
    msg <- c(
      glue("Each value of `{.column}` must be one of these:\n{toString(valid)}."),
      x = glue("You passed: {toString(setdiff(data[[column]], valid))}.")
    )
    abort(msg, class = "unknown_value")
  }

  invisible(data)
}
