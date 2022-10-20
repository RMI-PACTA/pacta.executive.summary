options(warn = -1)

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_fossil_bars(1))
})

test_that("if `data` has zero rows errors gracefully", {
  zero_row <- toy_data_fossil_bars[0L, ]
  expect_snapshot_error(plot_fossil_bars(zero_row))
})

test_that("with missing crucial columns errors gracefully", {
  data_wrong_names <- toy_data_fossil_bars
  colnames(data_wrong_names) <- c("bad1", "bad2", "bad3", "bad4", "bad5", "bad6", "bad7")
  expect_snapshot_error(plot_fossil_bars(data_wrong_names))
})

test_that("with wrong values of `tech` errors gracefully", {
  data_wrong_tech <- toy_data_fossil_bars
  data_wrong_tech$tech <- as.character(data_wrong_tech$tech)
  data_wrong_tech$tech[1] <- "bad"
  expect_snapshot_error(plot_fossil_bars(data_wrong_tech))
})

test_that("with non-numeric values of `perc_aum` errors gracefully", {
  data_wrong_perc_aum <- toy_data_fossil_bars
  data_wrong_perc_aum$perc_aum <- as.character(data_wrong_perc_aum$perc_aum)
  expect_snapshot_error(plot_fossil_bars(data_wrong_perc_aum))
})

test_that("with wrong values of `perc_aum` errors gracefully", {
  data_wrong_perc_aum <- toy_data_fossil_bars
  data_wrong_perc_aum$perc_aum <- data_wrong_perc_aum$perc_aum * 1000
  expect_snapshot_error(plot_fossil_bars(data_wrong_perc_aum))
})

test_that("with multiple indices per `asset_class` errors gracefully", {
  data_wrong <- toy_data_fossil_bars %>%
    filter(asset_class == "equity") %>%
    rbind(toy_data_fossil_bars %>% filter(asset_class == "bonds", entity == "index")) %>%
    mutate(asset_class = "equity")
  expect_snapshot_error(plot_fossil_bars(data_wrong))
})
