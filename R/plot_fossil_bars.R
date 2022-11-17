#' Create a bar plot with exposures to fossil fuels
#'
#' @param data A data frame. In principle, an output of `prep_fossil_bars()`.
#'   Requirements:
#'   * Must have columns: `entity_name`, `entity_type`,`asset_class`,`tech`,
#'   `perc_aum`.
#'   * `tech` column must only have following values: "coal", "oil", "gas"
#'   * `perc_aum` must be a percentage in decimal format, with values between 0
#'   and 1.
#'
#' @return an object of class "ggplot".
#' @export
#'
#' @examples
#' plot_fossil_bars(toy_data_fossil_bars)
plot_fossil_bars <- function(data) {
  env <- list(data = substitute(data))
  check_data_fossil_bars(data, env = env)

  data <- data %>%
    mutate(
      entity_name_title = r2dii.plot::to_title(.data$entity_name),
      entity_title = r2dii.plot::to_title(.data$entity),
      entity_title = factor(
        .data$entity_title,
        levels = r2dii.plot::to_title(c("index", "peers", "portfolio"))
      )
    )

  p <- ggplot(data, aes(x = .data$entity_title, y = .data$perc_aum, fill = .data$entity_type)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(
        y = .data$perc_aum,
        label = scales::percent(round(.data$perc_aum, digits = 4))
      ),
      hjust = -0.2,
      size = 4
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, .4))) +
    scale_fill_manual(
      values = fill_colours_fossil_bars,
      labels = fill_labels_fossil_bars,
    ) +
    coord_flip() +
    theme_2dii(base_size = 18) +
    theme(
      axis.title = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      strip.placement = "outside"
    ) +
    guides(fill = guide_legend(reverse = TRUE)) +
    facet_grid(tech ~ asset_class, labeller = as_labeller(r2dii.plot::to_title), switch = "y") +
    labs(
      caption = make_caption_indices(data)
    ) +
    theme(
      plot.caption = element_text(size = 10)
    )
  p
}

check_data_fossil_bars <- function(data, env) {
  stopifnot(is.data.frame(data))
  abort_if_has_zero_rows(data, env = env)
  abort_if_missing_names(
    data,
    c("asset_class", "tech", "entity_name", "entity_type", "perc_aum", "entity")
  )
  abort_if_invalid_values(data, "tech", c("coal", "oil", "gas"))
  stopifnot(is.numeric(data$perc_aum))
  stopifnot((data$perc_aum <= 1) & (data$perc_aum >= 0))
  check_single_index_per_asset_class(data, "entity_name", env = env)
}

check_single_index_per_asset_class <- function(data, column, env) {
  .data <- deparse_1(substitute(data, env = env))
  data <- data %>%
    filter(.data$entity == "index")
  if (nrow(data) > 0)
    asset_classes <- unique(data$asset_class)

    for (asset in asset_classes) {
        data_subset <- data %>%
          filter(.data$asset_class == asset)
        if (nrow(data_subset) > 0) {
        .column <- unique(data_subset[[column]])
        if (length(.column) > 1L) {
          abort(c(
            glue("`{.data}` must have a single value of `{column}` per asset class for `entity == 'index'`."),
            x = glue("Provided: {toString(.column)} for asset_class == {asset}.")
          ))
        }
      }
    }
}

make_caption_indices <- function(data) {
  caption <- "Indices used per asset class:\n"
  asset_classes <- unique(data$asset_class)
  last_asset <- asset_classes[length(asset_classes)]
  for (asset in asset_classes) {
    index_name <- data %>%
      filter(
        .data$asset_class == asset,
        .data$entity == "index"
      ) %>%
      pull(.data$entity_name) %>%
      unique()
    caption <- glue::glue(caption, "{r2dii.plot::to_title(asset)}: {r2dii.plot::to_title(index_name)}")
    if (asset != last_asset) {
      caption <- glue::glue(caption, ", ")
    } else {
      caption <- glue::glue(caption, ".")
    }
  }
  caption
}
