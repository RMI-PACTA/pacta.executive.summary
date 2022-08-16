options(warn = -1)

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_scores(1))
})

test_that("if `data` has zero rows errors gracefully", {
  zero_row <- toy_data_scores[0L, ]
  expect_snapshot_error(plot_scores(zero_row))
})

test_that("with missing crucial columns errors gracefully", {
  data_wrong_names <- toy_data_scores
  colnames(data_wrong_names) <- c("bad1", "bad2", "bad3", "bad4", "bad5")
  expect_snapshot_error(plot_scores(data_wrong_names))
})

test_that("with multiple values for `asset_class` errors gracefully", {
  expect_snapshot_error(plot_scores(toy_data_scores))
})

test_that("with wrong values of `scope` errors gracefully", {
  data_wrong <- toy_data_scores %>% dplyr::filter(asset_class == "bonds")
  data_wrong$scope <- as.character(data_wrong$scope)
  data_wrong$scope[1] <- "bad"
  expect_snapshot_error(plot_scores(data_wrong))
})

test_that("with missing crucial values in `entity` errors gracefully", {
  data_missing <- toy_data_scores %>% 
    dplyr::filter(asset_class == "bonds", entity != "this_portfolio")
  expect_snapshot_error(plot_scores(data_missing))
  
  data_missing <- toy_data_scores %>% 
    dplyr::filter(asset_class == "bonds", entity != "peers")
  expect_snapshot_error(plot_scores(data_missing))
})

test_that("with wrong values of `sector` errors gracefully", {
  data_wrong <- toy_data_scores %>% dplyr::filter(asset_class == "bonds")
  data_wrong$sector <- as.character(data_wrong$sector)
  data_wrong$sector[1] <- "bad"
  expect_snapshot_error(plot_scores(data_wrong))
})

test_that("with wrong values of `score` errors gracefully", {
  data_wrong <- toy_data_scores %>% dplyr::filter(asset_class == "bonds")
  data_wrong$score <- as.character(data_wrong$score)
  data_wrong$score[1] <- "bad"
  expect_snapshot_error(plot_scores(data_wrong))
})
