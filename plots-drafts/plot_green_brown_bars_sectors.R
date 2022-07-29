library(dplyr)
library(ggplot2)
library(r2dii.plot)
library(r2dii.colours)
library(tidytext)
library(patchwork)

data_sec <- readxl::read_excel(here::here("plots-drafts/toy-data/data_tech_green_brown_sectors.xlsx")) %>%
  filter(
    year == 2022,
    entity_name == "portfolio"
    ) %>%
  mutate(
    tech_exp = tech_mix_perc * exp_sector,
    #sector = r2dii.plot::to_title(sector),
    asset_type = factor(asset_type, levels = c("equity", "bonds"))
  )

data_summ <- data_sec %>%
  group_by(asset_type, sector, tech_type) %>%
  mutate(
    sum_exp = sum(tech_exp)
  ) %>%
  select(-tech, -tech_mix_perc, -tech_exp) %>%
  distinct() %>%
  ungroup() %>%
  mutate(
#    tech_type = r2dii.plot::to_title(tech_type),
    tech_type = factor(tech_type, levels = c("green", "hydro_and_nuclear", "brown", "other"))
   # sector_reordered = reorder_within(
    #  sector, exp_sector, asset_type
#    )
  ) %>%
  group_by(asset_type) %>%
  mutate(
    perc_tech_exposure = sum_exp/sum(sum_exp),
    perc_sec_exposure = exp_sector/sum(sum_exp)
  ) %>%
  select(-exp_sector, -sum_exp, -entity_name, -entity_type)

toy_data_green_brown_bars <- data_summ

usethis::use_data(toy_data_green_brown_bars)

colours <- c(
  "#7BC17E", "#b9b5b0", "#977447", "#6e819c"
)
names(colours) <- c("Green", "Hydro And Nuclear", "Brown", "Other")

p <- ggplot(data_summ, aes(x = sector_reordered, y = perc_tech_exposure, fill = tech_type)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(y = perc_sec_exposure, 
        label = scales::percent(round(perc_sec_exposure, digits = 2))), 
    hjust = -0.2,
    size = 7
    ) +
  scale_x_reordered() +
  scale_y_continuous(expand = expansion(mult = c(0, .4)), labels = scales::percent) +
  scale_fill_manual(
    values = colours, 
    labels = c("Green" = "Low-carbon", "Hydro And Nuclear" = "Hydro & Nuclear", "Brown" = "High-carbon", "Other" = "Sectors without\ntech. roadmap"),
    name = "Technology classification") +
  coord_flip() +
  theme_2dii(base_size = 28) +
  theme(
    axis.title = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  facet_wrap(~ asset_type, scale = "free", labeller = as_labeller(r2dii.plot::to_title))
p
ggsave(
  "../../visualisation/Executive-summary/exposure_sector_green_brown.png"
  )
