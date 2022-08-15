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
  env <- list(data = substitute(data))
  check_data_alignment_table(data, env = env)
  
  size_lim <- c(min(data$perc_aum, na.rm = TRUE), max(data$perc_aum, na.rm = TRUE))
  size_range <- c(4,12)
  
  p_power <- plot_alignment_table_sector_stripe(data, "power", size_lim, 
                                               size_range, ncol = 4)
  
  p_fossil <- plot_alignment_table_sector_stripe(data, "fossil_fuels", size_lim, 
                                               size_range, ncol = 3)
  
  p_auto <- plot_alignment_table_sector_stripe(data, "automotive", size_lim, 
                                               size_range, ncol = 3)
    
  p_ylabel = ggplot(data.frame(l = "Aligned scenario temperature", x = 1, y = 1)) +
          geom_text(aes(x = .data$x, y = .data$y, label = .data$l), angle = 90, size = 7) + 
          theme_void() +
          coord_cartesian(clip = "off")
  
  p_tech <- (p_power / p_fossil / p_auto) + 
    plot_layout(guides = "collect")
  
  p <- p_ylabel + p_tech + plot_layout(widths = c(1, 15))
  p
}

check_data_alignment_table <- function(data, env = env) {
  stopifnot(is.data.frame(data))
  abort_if_has_zero_rows(data, env = env)
  abort_if_missing_names(
    data,
    c("asset_class", "sector", "technology", "entity", "aligned_scen_temp", 
      "perc_aum")
  )
  abort_if_multiple(data, "asset_class", env)
  abort_if_missing_crucial_value(data, "entity", "portfolio", env)
  abort_if_invalid_values(data, "sector", c("power", "fossil_fuels", "automotive"))
  abort_if_invalid_values(data, "aligned_scen_temp", c(">3.2C", "2.7-3.2C", "<2C"))
  stopifnot(is.numeric(data$perc_aum))
  stopifnot((data$perc_aum <= 1) & (data$perc_aum >= 0))
}

plot_alignment_table_tech_cells <- function(data) {
  pj <- position_jitter(width = 0, height = 0.1, seed = 0)
  
  annotations <- make_annotations_df(data)
    
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
      values = c(">3.2C" = r2dii.colours::palette_2dii_scenario %>% 
                   filter(.data$label == "red") %>% 
                   pull(.data$hex),
                 "2.7-3.2C" = r2dii.colours::palette_2dii_scenario %>% 
                   filter(.data$label == "dark_yellow") %>% 
                   pull(.data$hex),
                 "<2C" = r2dii.colours::palette_2dii_scenario %>% 
                   filter(.data$label == "dark_green") %>% 
                   pull(.data$hex)
                 )
        ) +
    scale_color_manual(
      values = c(">3.2C" = unname(fill_colours_scores["E"]),
                 "2.7-3.2C" = unname(fill_colours_scores["C"]),
                 "<2C" = unname(fill_colours_scores["A+"])
      ),
      na.value = NA
      ) +
    scale_shape_manual(
      values = c(15, 0), 
      labels = as_function(~ r2dii.colours:::make_pretty_labels(.x))) +
    scale_x_discrete(
      limits = levels(data$entity),
      labels = as_function(~ r2dii.colours:::make_pretty_labels(.x))
      ) + 
    scale_y_discrete(
      expand = expansion(add = c(0.5, 0.5)),
      limits = c(">3.2C", "2.7-3.2C", "<2C")
    ) +
    theme_2dii(base_size = 20) +
    theme(
      legend.title = element_text(size = 16),
      axis.title.x = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.ticks = element_blank(),
      axis.text.x = element_blank()
      ) +
    guides(
      color = "none",
      fill = "none",
      size = guide_legend(order = 2, override.aes = list(shape = 15)), 
      shape = guide_legend(order = 1, override.aes = list(size = 5))
      )
  
  p
}

plot_alignment_table_sector_stripe <- function(
  data, 
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
    ~ technology, 
    labeller = as_labeller(as_function(~ r2dii.colours:::make_pretty_labels(.x))), 
    ncol = ncol
    )
  p
}

make_annotations_df <- function(data) {
  annotations_tech <- tibble::tibble(
    aligned_scen_temp = c(">3.2C", "2.7-3.2C", "<2C"),
    ymin = c(.5, 1.5, 2.5),
    ymax = c(1.5, 2.5, 3.5)
  )
  
  annotations <- unique(data %>% pull(.data$technology)) %>% 
    purrr::map_df(~ annotations_tech %>% mutate(technology = .x))
  
  data_portfolio <- data %>%
    filter(
      .data$entity == "portfolio"
      ) %>%
    mutate(
      aligned_portfolio_temp = .data$aligned_scen_temp
    ) %>% 
    select(
      .data$technology, .data$aligned_scen_temp, .data$entity, .data$aligned_portfolio_temp
    ) %>%
    distinct()
    
  annotations <- annotations %>% 
    left_join(data_portfolio) 
  
  annotations
}
