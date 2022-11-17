#' Create a diagram of asset class coverage
#'
#' @param data A data frame. In principle an output of `prep_diagram()`.
#'   Requirements:
#'   * Must have columns: `asset_class`, `exposure_portfolio`,
#'   `exposure_asset_class`, `exposure_asset_class_perc`, `exposure_pacta`,
#'   `exposure_pacta_perc_asset_class_exposure`, `emissions_pacta_perc`,
#'   `emissions_pacta`.
#'   * Must have two rows.
#'   * `asset_class` must have values: "equity", "bonds".
#'   * `exposure_portfolio`, `exposure_asset_class`, `exposure_asset_class_perc`,
#'   `exposure_pacta`, `exposure_pacta_perc_asset_class_exposure`,
#'   `emissions_pacta_perc`, `emissions_pacta` must be numeric.
#'   * `exposure_asset_class_perc`, `exposure_pacta_perc_asset_class_exposure`,
#'   `emissions_pacta_perc` must be a percentage in decimal format, with values
#'   between 0 and 1.
#'
#' @return An object of class "htmlwidget".
#'
#' @export
#'
#' @examples
#' plot_diagram(toy_data_diagram)

plot_diagram <- function(data = NULL) {
  tryCatch(
    {
      env <- list(data = substitute(data))
      check_data_diagram(data, env)

      data_bonds <- data %>%
        filter(.data$asset_class == "bonds")

      data_equity <- data %>%
        filter(.data$asset_class == "equity")

      currency_id <- "CHF"
      emissions_unit <- "tonnes"

      ttl_exp_lbl <- paste0(scales::label_comma()(data_equity$exposure_portfolio), " ", currency_id)

      eq_exp_lbl <-
        paste0(
          scales::percent(data_equity$exposure_asset_class_perc),
          "\n(",
          scales::label_comma()(data_equity$exposure_asset_class),
          " ", currency_id, ")"
        )

      cb_exp_lbl <-
        paste0(
          scales::percent(data_bonds$exposure_asset_class_perc),
          "\n(",
          scales::label_comma()(data_bonds$exposure_asset_class),
          " ", currency_id, ")"
        )

      eq_pacta_exp_lbl <-
        paste0(
          scales::percent(data_equity$exposure_pacta_perc_asset_class_exposure),
          "\n(",
          scales::label_comma()(data_equity$exposure_pacta),
          " ", currency_id, ")"
        )

      cb_pacta_exp_lbl <-
        paste0(
          scales::percent(data_bonds$exposure_pacta_perc_asset_class_exposure),
          "\n(",
          scales::label_comma()(data_bonds$exposure_pacta),
          " ", currency_id, ")"
        )

      eq_emiss_exp_lbl <-
        paste0(
          scales::percent(data_equity$emissions_pacta_perc),
          "\n(",
          scales::label_comma()(data_equity$emissions_pacta),
          " ", emissions_unit, ")"
        )

      cb_emiss_exp_lbl <-
        paste0(
          scales::percent(data_bonds$emissions_pacta_perc),
          "\n(",
          scales::label_comma()(data_bonds$emissions_pacta),
          " ", emissions_unit, ")"
        )

      box_width <- 15
      box_height <- 10
      arrow_x_length <- 10

      row_mid_y <- 20
      row_top_y <- 30
      row_bottom_y <- 10

      col_1_x <- 0

      box_linewidth <- 0.25
      arrow_linewidth <- 0.15
      arrow_length_mm <- 2

      box_text_offset_x <- box_width / 2
      box_text_offset_y <- box_height / 2
      box_label_offset_x <- box_width / 2
      box_label_offset_y <- box_height + 5

      col_2_x <- col_1_x + box_width + arrow_x_length
      col_3_x <- col_2_x + box_width + arrow_x_length
      col_4_x <- col_3_x + box_width + arrow_x_length

      arrow_label_offset_x <- box_width + (arrow_x_length / 2)
      arrow_label_offset_y <- row_top_y - row_mid_y + (box_height / 2) - 3
      arrow_label_angle <- 27

      p <- ggplot(tibble(x = 0:100, y = 0:100), aes(x = .data$x, y = .data$y)) +
        theme_void() +
        scale_y_continuous(limits = c(0, 50)) +
        # total exposure
        annotate("text", x = col_1_x + box_label_offset_x, y = row_mid_y + box_label_offset_y, label = "Total Portfolio Exposure", size = 2.5) +
        geom_rect(xmin = col_1_x, xmax = col_1_x + box_width,
                  ymin = row_mid_y, ymax = row_mid_y + box_height,
                  color = "black", fill = "white", linewidth = box_linewidth) +
        annotate("text", x = col_1_x + box_text_offset_x, y = row_mid_y + box_text_offset_y, label = ttl_exp_lbl, size = 2.5) +
        # first segments
        annotate("text", x = col_1_x + arrow_label_offset_x,
                 y = row_mid_y + arrow_label_offset_y,
                 label = "Listed Equity",
                 size = 2.5, angle = arrow_label_angle) +
        geom_segment(
          x = col_1_x + box_width, xend = col_2_x,
          y = row_mid_y + box_text_offset_y, yend = row_top_y + box_text_offset_y,
          linewidth = arrow_linewidth, linejoin = "mitre", lineend = "butt",
          arrow = arrow(length = unit(arrow_length_mm, "mm"), type= "closed")
        ) +
        annotate("text", x = col_1_x + arrow_label_offset_x,
                 y = row_mid_y - arrow_label_offset_y + box_height,
                 label = "Corporate Bonds",
                 size = 2.5, angle = -arrow_label_angle) +
        geom_segment(
          x = col_1_x + box_width, xend = col_2_x,
          y = row_mid_y + box_text_offset_y, yend = row_bottom_y + box_text_offset_y,
          linewidth = arrow_linewidth, linejoin = "mitre", lineend = "butt",
          arrow = arrow(length = unit(arrow_length_mm, "mm"), type= "closed")
        ) +
        # exposure per asset class
        annotate("text", x = col_2_x + box_label_offset_x, y = row_top_y + box_label_offset_y, label = "Exposure per asset class", size = 2.5) +
        # equity exposure
        geom_rect(xmin = col_2_x, xmax = col_2_x + box_width,
                  ymin = row_top_y, ymax = row_top_y + box_height,
                  color = "black", fill = "white", linewidth = box_linewidth) +
        annotate("text", x = col_2_x + box_text_offset_x, y = row_top_y + box_text_offset_y, label = eq_exp_lbl, size = 2.5) +
        # bond exposure
        geom_rect(xmin = col_2_x, xmax = col_2_x + box_width,
                  ymin = row_bottom_y, ymax = row_bottom_y + box_height,
                  color = "black", fill = "white", linewidth = box_linewidth) +
        annotate("text", x = col_2_x + box_text_offset_x, y = row_bottom_y + box_text_offset_y, label = cb_exp_lbl, size = 2.5) +
        # second segments
        geom_segment(
          x = col_2_x + box_width, xend = col_3_x,
          y = row_top_y + box_text_offset_y, yend = row_top_y + box_text_offset_y,
          linewidth = arrow_linewidth, linejoin = "mitre", lineend = "butt",
          arrow = arrow(length = unit(arrow_length_mm, "mm"), type= "closed")
        ) +
        geom_segment(
          x = col_2_x + box_width, xend = col_3_x,
          y = row_bottom_y + box_text_offset_y, yend = row_bottom_y + box_text_offset_y,
          linewidth = arrow_linewidth, linejoin = "mitre", lineend = "butt",
          arrow = arrow(length = unit(arrow_length_mm, "mm"), type= "closed")
        ) +
        # exposure to PACTA sectors per asset class
        annotate("text", x = col_3_x + box_label_offset_x, y = row_top_y + box_label_offset_y, label = "Exposure covered by PACTA sectors*\n(as % of asset class exposure", size = 2.5) +
        # equity PACTA exposure
        geom_rect(xmin = col_3_x, xmax = col_3_x + box_width,
                  ymin = row_top_y, ymax = row_top_y + box_height,
                  color = "black", fill = "white", linewidth = box_linewidth) +
        annotate("text", x = col_3_x + box_text_offset_x, y = row_top_y + box_text_offset_y, label = eq_pacta_exp_lbl, size = 2.5) +
        # bond PACTA exposure
        geom_rect(xmin = col_3_x, xmax = col_3_x + box_width,
                  ymin = row_bottom_y, ymax = row_bottom_y + box_height,
                  color = "black", fill = "white", linewidth = box_linewidth) +
        annotate("text", x = col_3_x + box_text_offset_x, y = row_bottom_y + box_text_offset_y, label = cb_pacta_exp_lbl, size = 2.5) +
        # third segments
        geom_segment(
          x = col_3_x + box_width, xend = col_4_x,
          y = row_top_y + box_text_offset_y, yend = row_top_y + box_text_offset_y,
          linewidth = arrow_linewidth, linejoin = "mitre", lineend = "butt",
          arrow = arrow(length = unit(arrow_length_mm, "mm"), type= "closed")
        ) +
        geom_segment(
          x = col_3_x + box_width, xend = col_4_x,
          y = row_bottom_y + box_text_offset_y, yend = row_bottom_y + box_text_offset_y,
          linewidth = arrow_linewidth, linejoin = "mitre", lineend = "butt",
          arrow = arrow(length = unit(arrow_length_mm, "mm"), type= "closed")
        ) +
        # emissions per asset class
        annotate("text", x = col_4_x + box_label_offset_x, y = row_top_y + box_label_offset_y, label = "Emissions covered by PACTA sectors*\n(as % of asset class emissions", size = 2.5) +
        # equity emissions
        geom_rect(xmin = col_4_x, xmax = col_4_x + box_width,
                  ymin = row_top_y, ymax = row_top_y + box_height,
                  color = "black", fill = "white", linewidth = box_linewidth) +
        annotate("text", x = col_4_x + box_text_offset_x, y = row_top_y + box_text_offset_y, label = eq_emiss_exp_lbl, size = 2.5) +
        # bond PACTA exposure
        geom_rect(xmin = col_4_x, xmax = col_4_x + box_width,
                  ymin = row_bottom_y, ymax = row_bottom_y + box_height,
                  color = "black", fill = "white", linewidth = box_linewidth) +
        annotate("text", x = col_4_x + box_text_offset_x, y = row_bottom_y + box_text_offset_y, label = cb_emiss_exp_lbl, size = 2.5)
    },
    error = function (e) {
      cat("There was an error in plot_diagram().\nReturning empty plot object.\n")
      p <- empty_plot_error_message()
    }
  )
  p
}

check_data_diagram <- function(data, env) {
  stopifnot(is.data.frame(data))
  stopifnot(nrow(data) == 2)
  abort_if_missing_names(
    data,
    c(
      "exposure_portfolio", "asset_class", "exposure_asset_class",
      "exposure_asset_class_perc", "exposure_pacta",
      "exposure_pacta_perc_asset_class_exposure", "emissions_pacta_perc",
      "emissions_pacta"
    )
  )
  abort_if_missing_crucial_values(data, "asset_class", c("equity", "bonds"), env)
  stopifnot(is.numeric(data$exposure_portfolio))
  stopifnot(is.numeric(data$exposure_asset_class))
  stopifnot(is.numeric(data$exposure_asset_class_perc))
  stopifnot(is.numeric(data$exposure_pacta))
  stopifnot(is.numeric(data$exposure_pacta_perc_asset_class_exposure))
  stopifnot(is.numeric(data$emissions_pacta))
  stopifnot(is.numeric(data$emissions_pacta_perc))
  stopifnot((data$exposure_asset_class_perc <= 1) & (data$exposure_asset_class_perc >= 0))
  stopifnot((data$exposure_pacta_perc_asset_class_exposure <= 1) & (data$exposure_pacta_perc_asset_class_exposure >= 0))
  stopifnot((data$emissions_pacta_perc <= 1) & (data$emissions_pacta_perc >= 0))
}
