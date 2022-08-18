options(warn = -1)

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_diagram(1))
})

test_that("if `data` does not have 2 rows errors gracefully", {
  one_row <- toy_data_diagram[1L, ]
  expect_snapshot_error(plot_diagram(one_row))
})

test_that("with missing crucial columns errors gracefully", {
  data_wrong_names <- toy_data_diagram
  colnames(data_wrong_names) <- c("bad1", "bad2", "bad3", "bad4", "bad5", "bad6", "bad7", "bad8")
  expect_snapshot_error(plot_diagram(data_wrong_names))
})

test_that("with missing crucial values of `asset_class` errors gracefully", {
  data_wrong <- toy_data_diagram
  data_wrong$asset_class <- c("bad1", "bad2")
  expect_snapshot_error(plot_diagram(data_wrong))
})

test_that("with non-numeric values of numeric columns errors gracefully", {
  data_wrong <- toy_data_diagram %>%
    mutate(
      exposure_portfolio = as.character(.data$exposure_portfolio)
    )
  expect_snapshot_error(plot_diagram(data_wrong))
  
  data_wrong <- toy_data_diagram %>%
    mutate(
      exposure_asset_class = as.character(.data$exposure_asset_class)
    )
  expect_snapshot_error(plot_diagram(data_wrong))
  
  data_wrong <- toy_data_diagram %>%
    mutate(
      exposure_asset_class_perc = as.character(.data$exposure_asset_class_perc)
    )
  expect_snapshot_error(plot_diagram(data_wrong))
  
  data_wrong <- toy_data_diagram %>%
    mutate(
      exposure_pacta = as.character(.data$exposure_pacta)
    )
  expect_snapshot_error(plot_diagram(data_wrong))
  
  data_wrong <- toy_data_diagram %>%
    mutate(
      exposure_pacta_perc_asset_class_exposure = as.character(.data$exposure_pacta_perc_asset_class_exposure)
    )
  expect_snapshot_error(plot_diagram(data_wrong))
  
  data_wrong <- toy_data_diagram %>%
    mutate(
      emissions_pacta = as.character(.data$emissions_pacta)
    )
  expect_snapshot_error(plot_diagram(data_wrong))
  
  data_wrong <- toy_data_diagram %>%
    mutate(
      emissions_pacta_perc = as.character(.data$emissions_pacta_perc)
    )
  expect_snapshot_error(plot_diagram(data_wrong))
})

test_that("with wrong values of percentage columns errors gracefully", {
  data_wrong <- toy_data_diagram %>%
    mutate(
      exposure_asset_class_perc = .data$exposure_asset_class_perc * 1000 
      )
  expect_snapshot_error(plot_diagram(data_wrong))
  
  data_wrong <- toy_data_diagram %>%
    mutate(
      exposure_pacta_perc_asset_class_exposure = .data$exposure_pacta_perc_asset_class_exposure * 1000 
      )
  expect_snapshot_error(plot_diagram(data_wrong))
  
  data_wrong <- toy_data_diagram %>%
    mutate(
      emissions_pacta_perc = .data$emissions_pacta_perc * 1000 
      )
  expect_snapshot_error(plot_diagram(data_wrong))
})