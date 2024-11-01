#' Create a bar plot with exposures to low and high carbon technologies
#'
#' @param data A data frame. In principle, an output of
#'   `prep_green_brown_bars()`. Requirements:
#'   * Must have columns: `asset_class`,`tech_type`, `sector`,
#'   `perc_sec_exposure`, `perc_tech_exposure`.
#'   * `tech_type` column must only have following values: "green",
#'   "nuclear", "brown", "other".
#'   * `perc_tech_exposure` and `perc_sec_exposure` must be percentages in
#'   decimal format, with values between 0 and 1.
#'
#' @return an object of class "ggplot".
#' @export
#'
#' @examples
#' plot_green_brown_bars(toy_data_green_brown_bars)
plot_green_brown_bars <- function(data) {
  env <- list(data = substitute(data))
  check_data_green_brown_bars(data, env = env)

  data <- data %>%
    mutate(
      tech_type = r2dii.plot::to_title(.data$tech_type),
      tech_type = factor(
        .data$tech_type,
        levels = r2dii.plot::to_title(c("green", "nuclear", "brown", "other"))
      ),
      sector = r2dii.plot::to_title(.data$sector),
      sector_reordered = tidytext::reorder_within(.data$sector, .data$perc_sec_exposure, .data$asset_class)
    )

  p <- ggplot(data, aes(x = .data$sector_reordered, y = .data$perc_tech_exposure, fill = .data$tech_type)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(
        y = .data$perc_sec_exposure,
        label = scales::percent(round(.data$perc_sec_exposure, digits = 3))
      ),
      hjust = -0.2,
      size = 3.5
    ) +
    tidytext::scale_x_reordered() +
    scale_y_continuous(expand = expansion(mult = c(0, .55)), labels = scales::percent) +
    scale_fill_manual(
      values = fill_colours_green_brown_bars,
      labels = fill_labels_green_brown_bars,
    ) +
    coord_flip() +
    theme_2dii(base_size = 14) +
    theme(
      axis.title = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(nrow = 2)) +
    facet_wrap(~asset_class, scales = "free", labeller = as_labeller(r2dii.plot::to_title))
  p
}

check_data_green_brown_bars <- function(data, env) {
  stopifnot(is.data.frame(data))
  abort_if_has_zero_rows(data, env = env)
  abort_if_missing_names(
    data,
    c("asset_class", "tech_type", "sector", "perc_sec_exposure", "perc_tech_exposure")
  )
  abort_if_invalid_values(data, "tech_type", c("green", "nuclear", "brown", "other"))
  stopifnot(is.numeric(data$perc_tech_exposure))
  stopifnot(is.numeric(data$perc_sec_exposure))
  stopifnot((data$perc_tech_exposure <= 1) & (data$perc_tech_exposure >= 0))
  stopifnot((data$perc_sec_exposure <= 1) & (data$perc_sec_exposure >= 0))
}
