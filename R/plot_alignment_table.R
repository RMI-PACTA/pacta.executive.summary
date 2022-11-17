#' Create a scenario alignment table
#'
#' @param data A data frame. In principle, an output of
#'   `prep_alignment_table()`. Requirements:
#'   * `asset_class` must have a single value.
#'   * Must have columns: `asset_class`,`sector`,`technology`, `entity`,
#'    `aligned_scen_temp`, `perc_aum`.
#'   * `entity` must contain at least one value of "portfolio".
#'   * `sector` must be one of: "power", "fossil_fuels", "automotive".
#'   * `aligned_scen_temp` must be one of: ">3.2C", "2.7-3.2C", "<2C".
#'   * `perc_aum` must be a percentage in decimal format, with values
#'   between 0 and 1.
#'
#' @return an object of class "ggplot".
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' plot_alignment_table(toy_data_alignment_table %>% filter(asset_class == "equity"))
plot_alignment_table <- function(data) {
  stopifnot(is.data.frame(data))
  if (nrow(data) > 0) {
    env <- list(data = substitute(data))
    check_data_alignment_table(data, env = env)

    size_lim <- c(min(data$perc_aum, na.rm = TRUE), max(data$perc_aum, na.rm = TRUE))
    size_range <- c(1, 5)

    if (nrow(data %>% filter(.data$sector == "power")) > 0) {
      p_power <- plot_alignment_table_sector_stripe(data, "power", size_lim,
        size_range,
        ncol = 4
      )
    } else {
      p_power <- patchwork::plot_spacer()
    }

    if (nrow(data %>% filter(.data$sector == "fossil_fuels")) > 0) {
      p_fossil <- plot_alignment_table_sector_stripe(data, "fossil_fuels", size_lim,
        size_range,
        ncol = 3
      )
    } else {
      p_fossil <- patchwork::plot_spacer()
    }

    if (nrow(data %>% filter(.data$sector == "automotive")) > 0) {
      p_auto <- plot_alignment_table_sector_stripe(data, "automotive", size_lim,
        size_range,
        ncol = 3
      )
    } else {
      p_auto <- patchwork::plot_spacer()
    }


    p_ylabel <- ggplot(data.frame(l = "Aligned scenario temperature", x = 1, y = 1)) +
      geom_text(aes(x = .data$x, y = .data$y, label = .data$l), angle = 90, size = 4) +
      theme_void() +
      coord_cartesian(clip = "off")

    p_tech <- (p_power / p_fossil / p_auto) +
      plot_layout(guides = "collect")

    p <- p_ylabel + p_tech + plot_layout(widths = c(1, 15))
  } else {
    p <- empty_plot_no_data_message()
  }
  p
}

check_data_alignment_table <- function(data, env = env) {
  abort_if_has_zero_rows(data, env = env)
  abort_if_missing_names(
    data,
    c(
      "asset_class", "sector", "technology", "entity", "aligned_scen_temp",
      "perc_aum"
    )
  )
  abort_if_multiple(data, "asset_class", env)
  abort_if_missing_crucial_values(data, "entity", c("portfolio"), env)
  abort_if_invalid_values(data, "sector", c("power", "fossil_fuels", "automotive"))
  abort_if_invalid_values(data, "aligned_scen_temp", alignment_table_temperatures_lookup)
  stopifnot(is.numeric(data$perc_aum))
  stopifnot((data$perc_aum <= 1) & (data$perc_aum >= 0))
}

plot_alignment_table_tech_cells <- function(data) {
  pj <- position_jitter(width = 0, height = 0.1, seed = 0)

  annotations <- make_annotations_df(data)

  fill_values <- c(r2dii.colours::palette_2dii_scenario %>%
          filter(.data$label == "dark_green") %>%
          pull(.data$hex), r2dii.colours::palette_2dii_scenario %>%
          filter(.data$label == "dark_yellow") %>%
          pull(.data$hex), r2dii.colours::palette_2dii_scenario %>%
          filter(.data$label == "red") %>%
          pull(.data$hex))
  names(fill_values) <- alignment_table_temperatures_lookup
  colour_values <- c(unname(fill_colours_scores["A+"]), unname(fill_colours_scores["C"]), unname(fill_colours_scores["E"]))
  names(colour_values) <- alignment_table_temperatures_lookup

  data <- data %>%
    mutate(
      entity = factor(.data$entity, levels = c("portfolio", "peers") )
    )

  p <- ggplot(
    data,
    aes(
      x = .data$entity,
      y = .data$aligned_scen_temp,
      group = .data$technology
    )
  ) +
    geom_rect(
      data = annotations,
      xmin = -Inf,
      xmax = Inf,
      alpha = 0.25,
      size = 1,
      aes(
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$aligned_scen_temp,
        color = .data$aligned_portfolio_temp
      )
    ) +
    geom_point(
      aes(shape = .data$entity, size = .data$perc_aum),
      position = pj
    ) +
    scale_fill_manual(
      values = fill_values
    ) +
    scale_color_manual(
      values = colour_values,
      na.value = NA
    ) +
    scale_shape_manual(
      values = c(15, 0),
      labels = as_function(~ r2dii.colours:::make_pretty_labels(.x))
    ) +
    scale_x_discrete(
      limits = levels(data$entity),
      labels = as_function(~ r2dii.colours:::make_pretty_labels(.x))
    ) +
    scale_y_discrete(
      expand = expansion(add = c(0.5, 0.5)),
      limits = rev(alignment_table_temperatures_lookup)
    ) +
    theme_2dii(base_size = 10) +
    theme(
      legend.title = element_text(size = 8),
      axis.title.x = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "in"),
    ) +
    guides(
      color = "none",
      fill = "none",
      size = guide_legend(order = 2, override.aes = list(shape = 15)),
      shape = guide_legend(order = 1, override.aes = list(size = 3))
    )

  p
}

plot_alignment_table_sector_stripe <- function(data,
                                               sector,
                                               size_lim,
                                               size_range,
                                               ncol = 3) {
  p <- plot_alignment_table_tech_cells(
    data %>% filter(.data$sector == .env$sector)
  ) +
    scale_size_continuous(
      limits = size_lim,
      range = size_range,
      labels = scales::percent
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = r2dii.plot::to_title(sector),
      size = "Exposure\n(% of AUM)",
      shape = "Entity"
    ) +
    theme(axis.title.y = element_blank()) +
    facet_wrap(
      ~technology,
      labeller = as_labeller(as_function(~ r2dii.colours:::make_pretty_labels(.x))),
      ncol = ncol
    )
  p
}

make_annotations_df <- function(data) {
  annotations_tech <- tibble::tibble(
    aligned_scen_temp = rev(alignment_table_temperatures_lookup),
    ymin = c(.5, 1.5, 2.5),
    ymax = c(1.5, 2.5, 3.5)
  )

  annotations <- unique(data %>% pull("technology")) %>%
    purrr::map_df(~ annotations_tech %>% mutate(technology = .x))

  data_portfolio <- data %>%
    filter(
      .data$entity == "portfolio"
    ) %>%
    mutate(
      aligned_portfolio_temp = .data$aligned_scen_temp
    ) %>%
    select(
      c("technology", "aligned_scen_temp", "entity", "aligned_portfolio_temp")
    ) %>%
    distinct()

  annotations <- annotations %>%
    left_join(data_portfolio, by = c("aligned_scen_temp", "technology"))

  annotations
}
