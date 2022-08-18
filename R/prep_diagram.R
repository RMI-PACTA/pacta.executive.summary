prep_diagram <- function(audit_data = NULL, emissions_data = NULL) {
  if (is.null(audit_data) | is.null(emissions_data)) {
    data_out <- use_toy_data("diagram")
  }
  data_out
}
