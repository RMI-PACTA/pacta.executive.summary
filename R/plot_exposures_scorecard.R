plot_exposures_scorecard <- function(data) {
  colours <- c(
    "#7BC17E", "#181716", "#4e3b37", "#977447"
  )
  names(colours) <- c("renewables_power", "other_fossil_fuels", "coal", "fossil_power")
  
  p <- ggplot(data, aes(x = sector_or_tech, y = exposure_perc_aum, fill = sector_or_tech)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(y = exposure_perc_aum, 
          label = scales::percent(round(exposure_perc_aum, digits = 2))), 
      hjust = -0.2,
      size = 7
      ) +
    scale_x_discrete(labels = r2dii.plot::to_title) + 
    scale_y_continuous(expand = expansion(mult = c(0, .8)), labels = scales::percent) +
    scale_fill_manual(values = colours) +
    coord_flip() +
    theme_2dii(base_size = 24) +
    theme(
      axis.title = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none"
    ) +
    facet_wrap(~asset_class, labeller = as_labeller(r2dii.plot::to_title))
  p
}
