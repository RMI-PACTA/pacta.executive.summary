options(warn = -1)

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_scores_scorecard_real_estate(1))
})

test_that("if `data` has zero rows errors gracefully", {
  zero_row <- toy_data_scores_scorecard_re[0L, ]
  expect_snapshot_error(plot_scores_scorecard_real_estate(zero_row))
})

test_that("with missing crucial columns errors gracefully", {
  data_wrong_names <- toy_data_scores_scorecard_re
  colnames(data_wrong_names) <- c("bad1", "bad2")
  expect_snapshot_error(plot_scores_scorecard_real_estate(data_wrong_names))
})

test_that("with wrong values of `asset_class` errors gracefully", {
  data_wrong <- toy_data_scores_scorecard_re
  data_wrong$asset_class <- as.character(data_wrong$asset_class)
  data_wrong$asset_class[1] <- "bad"
  expect_snapshot_error(plot_scores_scorecard_real_estate(data_wrong))
})

test_that("with wrong values of `score` errors gracefully", {
  data_wrong <- toy_data_scores_scorecard_re
  data_wrong$score <- as.character(data_wrong$score)
  data_wrong$score[1] <- "bad"
  expect_snapshot_error(plot_scores_scorecard_real_estate(data_wrong))
})
