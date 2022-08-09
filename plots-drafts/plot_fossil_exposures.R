library(dplyr)
library(ggplot2)
library(r2dii.plot)
library(r2dii.colours)
library(tidytext)
library(patchwork)

data <- readxl::read_excel(here::here("plots-drafts/toy-data/data_exposure_fossil.xlsx")) %>%
  mutate(
    asset_class = factor(asset_type, levels = c("equity", "bonds")),
    tech = factor(tech, levels = c("coal", "oil", "gas")),
    entity_type = factor(entity_type, levels = c("this_portfolio", "benchmark")),
    entity_name_title = r2dii.plot::to_title(entity_name),
    entity_name_title = factor(entity_name_title, levels = r2dii.plot::to_title(c("MSCI_world", "peers", "portfolio")))
  )

toy_data_fossil_bars <- data %>%
  select(-sector, -entity_name_title, -asset_type)

usethis::use_data(toy_data_fossil_bars, overwrite = TRUE)

colours_fossil <- c("#181716", "#c5c4cf")
names(colours_fossil) <- c("this_portfolio", "benchmark")

q <- ggplot(data, aes(x = entity_name_title, y = perc_aum, fill = entity_type)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(y = perc_aum, 
        label = scales::percent(round(perc_aum, digits = 4))), 
    hjust = -0.2,
    size = 7
    ) +
  scale_y_continuous(expand = expansion(mult = c(0, .4))) +
  scale_fill_manual(
    values = colours_fossil, 
    labels = c("this_portfolio" = "Portfolio", "benchmark" = "Benchmarks"),
    name = "Technology classification") +
  coord_flip() + 
  theme_2dii(base_size = 28) +
  theme(
    axis.title = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    strip.placement = "outside"
  ) +
  facet_grid(tech~asset_type, labeller = as_labeller(r2dii.plot::to_title), switch = "y")
q
ggsave("../../visualisation/Executive-summary/exposure_fossil.png")
