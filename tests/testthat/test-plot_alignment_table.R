options(warn = -1)

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_alignment_table(1))
})

test_that("if `data` has zero rows returns empty plot", {
  zero_row <- toy_data_alignment_table[0L, ]
  p <- plot_alignment_table(zero_row)
  expect_equal(p, empty_plot_no_data_message())
})

test_that("with missing crucial columns errors gracefully", {
  data_wrong_names <- toy_data_alignment_table
  colnames(data_wrong_names) <- c("bad1", "bad2", "bad3", "bad4", "bad5", "bad6", "bad7")
  expect_snapshot_error(plot_alignment_table(data_wrong_names))
})

test_that("with multiple values for `asset_class` errors gracefully", {
  expect_snapshot_error(plot_alignment_table(toy_data_alignment_table))
})

test_that("with missing crucial values in `entity` errors gracefully", {
  data_missing <- toy_data_alignment_table %>%
    dplyr::filter(asset_class == "bonds", entity != "portfolio")
  expect_snapshot_error(plot_alignment_table(data_missing))
})

test_that("with wrong values of `sector` errors gracefully", {
  data_wrong <- toy_data_alignment_table %>% dplyr::filter(asset_class == "bonds")
  data_wrong$sector <- as.character(data_wrong$sector)
  data_wrong$sector[1] <- "bad"
  expect_snapshot_error(plot_alignment_table(data_wrong))
})

test_that("with wrong values of `aligned_scen_temp` errors gracefully", {
  data_wrong <- toy_data_alignment_table %>% dplyr::filter(asset_class == "bonds")
  data_wrong$aligned_scen_temp <- as.character(data_wrong$aligned_scen_temp)
  data_wrong$aligned_scen_temp[1] <- "bad"
  expect_snapshot_error(plot_alignment_table(data_wrong))
})

test_that("with non-numeric values of `perc_aum` errors gracefully", {
  data_wrong <- toy_data_alignment_table %>% dplyr::filter(asset_class == "bonds")
  data_wrong$perc_aum <- as.character(data_wrong$perc_aum)
  expect_snapshot_error(plot_alignment_table(data_wrong))
})

test_that("with wrong values of `perc_aum` errors gracefully", {
  data_wrong <- toy_data_alignment_table %>% dplyr::filter(asset_class == "bonds")
  data_wrong$perc_aum <- data_wrong$perc_aum * 10000
  expect_snapshot_error(plot_alignment_table(data_wrong))
})
