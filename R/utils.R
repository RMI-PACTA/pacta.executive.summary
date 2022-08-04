use_toy_data <-function(plot_name) {
  data_out <- get(glue::glue("toy_data_",plot_name))
  rlang::warn("No input data provided. Using toy data as output.")
  data_out
}