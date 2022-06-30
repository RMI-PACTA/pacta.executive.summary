library(ggplot2)
library(dplyr)
library(rlang)
library(r2dii.plot)
library(r2dii.colours)
library(patchwork)
source('plot_scen_alignment_table.R')
source('plot_scen_alignment_table_all_sectors.R')

levels_scen_temp <- c('>3.2C', '2.7-3.2C', '2-2.7C', '<2C')

data_long <- readxl::read_excel(here::here('plots-drafts/toy-data/data_scen.xlsx')) %>%
  mutate(
    aligned_scen_temp = factor(aligned_scen_temp, levels = levels_scen_temp),
    entity = factor(entity, levels = c('portfolio', 'peers'))
  ) %>%
  group_by(sector, entity) %>%
  mutate(
    perc_exposure = exposure / sum(exposure),
    perc_exposure_port = exposure_port / sum(exposure_port)
  ) %>%
  arrange(
    entity, aligned_scen_temp
  ) %>%
  group_by(asset_class) %>%
  mutate(
    perc_aum = exposure / sum(exposure)
  )

# Green brown facteted

data <- data_long %>%
  mutate(
    green_brown = if_else(technology %in% c("electric", "renewables"), "Low-carbon", "High-carbon")
  )

plot_scen_alignment_table_all_sectors(data)
#ggsave("../../../visualisation/Executive-summary/scenario-alignement-tech3.png")