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
    sector = r2dii.plot::to_title(sector),
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
    tech_type = r2dii.plot::to_title(tech_type),
    tech_type = factor(tech_type, levels = c("Green", "Neutral", "Brown")),
    sector_reordered = reorder_within(
      sector, exp_sector, asset_type
    )
  ) %>%
  group_by(asset_type) %>%
  mutate(
    perc_tech_exposure = sum_exp/sum(sum_exp),
    perc_sec_exposure = exp_sector/sum(sum_exp)
  )

colours <- c(
  "#7BC17E", "#b9b5b0", "#181716", "#4e3b37", "#977447", "#977447"
)
names(colours) <- c("Green","Neutral", "Fossil Fuels", "Coal", "Other Brown", "Brown")

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
    labels = c("Green" = "Low-carbon", "Neutral" = "Unspecified", "Brown" = "High-carbon"),
    name = "Technology classification") +
  coord_flip() +
  theme_2dii(base_size = 24) +
  theme(
    axis.title = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  facet_wrap(~ asset_type, scale = "free", labeller = as_labeller(r2dii.plot::to_title)) + labs(
  title = "Sectoral exposure to\nhigh- and low-carbon technologies",
  subtitle = "Within PACTA sectors"
)
p

data_brown <- data_sec %>%
  group_by(asset_type) %>%
  mutate(
    perc_tech_exposure = tech_exp/sum(tech_exp),
    perc_sec_exposure = exp_sector/sum(tech_exp)
  ) %>%
  filter(
    sector == "Fossil Fuels"
  )

q <- ggplot(data_brown, aes(x = sector, y = perc_tech_exposure, fill = tech)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(y = perc_sec_exposure, 
        label = scales::percent(round(perc_sec_exposure, digits = 2))), 
    hjust = -0.2,
    size = 7
    ) +
  scale_x_reordered() +
  scale_y_continuous(expand = expansion(mult = c(0, .8)), labels = scales::percent) +
  scale_fill_2dii(palette = "pacta") +
  coord_flip() +
  theme_2dii(base_size = 24) +
  theme(
    axis.title = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom"
  ) +
  facet_wrap(~ asset_type, scale = "free") +
  labs(
    subtitle = "In particular"
  )
q

p/q + plot_layout(heights = c(4,1))
#ggsave("../../../visualisation/Executive-summary/exposure_sector.png")

data_brown <- data_brown %>%
  mutate(
    tech_title = r2dii.plot::to_title(tech),
    tech_reordered = reorder_within(
      tech_title, tech_exp, asset_type
    )
  )

q2 <- ggplot(data_brown, aes(x = tech_reordered, y = tech_exp/ 10^3, fill = tech)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(y = tech_exp/ 10^3, 
        label = paste0(as.character(tech_exp/ 10^3), "k")), 
    hjust = -0.2,
    size = 7
    ) +
  scale_x_reordered() +
  scale_y_continuous(expand = expansion(mult = c(0, .8)), labels = scales::percent) +
  scale_fill_2dii(palette = "pacta") +
  coord_flip() +
  theme_2dii(base_size = 24) +
  theme(
    axis.title = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(~ asset_type, scale = "free") +
  labs(
    subtitle = "In particular, technology exposure within Fossil Fuels:"
  )
q2

p / q2 + plot_layout(heights = c(3,1))
#ggsave("../../../visualisation/Executive-summary/exposure_sector2.png")
