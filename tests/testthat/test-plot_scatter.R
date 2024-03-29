options(warn = -1)

test_that("if `data` is not a data frame errors gracefully", {
  expect_snapshot_error(plot_scatter(1))
})

test_that("if `data` has zero rows returns empty plot", {
  zero_row <- toy_data_scatter[0L, ]
  p <- plot_alignment_table(zero_row)
  expect_equal(p, empty_plot_no_data_message())
})

test_that("with missing crucial columns errors gracefully", {
  data_wrong_names <- toy_data_scatter
  colnames(data_wrong_names) <- c("bad1", "bad2", "bad3", "bad4", "bad5", "bad6")
  expect_snapshot_error(plot_scatter(data_wrong_names))
})

test_that("with multiple values for `asset_class` errors gracefully", {
  expect_snapshot_error(plot_scatter(toy_data_scatter))
})

test_that("with wrong values of `entity_type` errors gracefully", {
  data_wrong_entity_type <- toy_data_scatter %>% dplyr::filter(asset_class == "bonds")
  data_wrong_entity_type$entity_type <- as.character(data_wrong_entity_type$entity_type)
  data_wrong_entity_type$entity_type[1] <- "bad"
  expect_snapshot_error(plot_scatter(data_wrong_entity_type))
})

test_that("with missing values of `entity_type` errors gracefully", {
  data_wrong <- toy_data_scatter %>% 
    dplyr::filter(
      asset_class == "bonds",
      entity_type != "peers"
      )
  expect_snapshot_error(plot_scatter(data_wrong))
})

test_that("with wrong values of `score` errors gracefully", {
  data_wrong <- toy_data_scatter %>% dplyr::filter(asset_class == "bonds")
  data_wrong$score <- as.character(data_wrong$score)
  data_wrong$score[1] <- "bad"
  expect_snapshot_error(plot_scatter(data_wrong))
})

test_that("with non-numeric values of `tech_mix_green` errors gracefully", {
  data_wrong <- toy_data_scatter %>% dplyr::filter(asset_class == "bonds")
  data_wrong$tech_mix_green <- as.character(data_wrong$tech_mix_green)
  expect_snapshot_error(plot_scatter(data_wrong))
})

test_that("with wrong values of `tech_mix_green` and `score` errors gracefully", {
  data_wrong <- toy_data_scatter %>% dplyr::filter(asset_class == "bonds")
  data_wrong$tech_mix_green <- data_wrong$tech_mix_green * 10000
  expect_snapshot_error(plot_scatter(data_wrong))
})
