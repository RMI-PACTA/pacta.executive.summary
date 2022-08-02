library(dplyr)
library(ggplot2)
library(ggExtra)
library(r2dii.plot)
library(r2dii.colours)
library(patchwork)
library(svglite)
library(here)


data <- readxl::read_excel(here("plots-drafts/toy-data/data_score_exp.xlsx"))

data_avg <- data %>%
  group_by(asset_type, year) %>%
  summarise(
    tech_mix_brown = mean(tech_mix_brown, na.rm = TRUE),
    tech_mix_green = mean(tech_mix_green, na.rm = TRUE),
    score = mean(score, na.rm = TRUE)
  ) %>%
  mutate(entity_name = "average", entity_type = "average") %>%
  bind_rows(data) %>%
  filter(entity_type != "all") %>%
  mutate(
    entity_type = factor(entity_type, levels = c("this_portfolio", "average", "peer", "benchmark"))
  ) 

data_fut <- data_avg %>%
  filter(year == 2027) 

data_fut <- data_fut %>%
  mutate(asset_type = "equity") %>%
  bind_rows(data_fut) %>%
  select(-tech_mix_brown)

toy_data_scatter <- data_fut
usethis::use_data(toy_data_scatter)

scores <-  readxl::read_excel(here("plots-drafts/toy-data/score_input.xlsx")) %>%
  mutate(
    score_delta = if_else(is.na(lag(score_upper)), score_upper, score_upper - lag(score_upper)),
    score_label = if_else(is.na(lag(score_upper)), 0, lag(score_upper)) + score_delta/2
  )

alignment_scores_values <- scores

usethis::use_data(alignment_scores_values)

colours_scores <- c(
  "#FF0D0D", "#FF4E11", "#FF8E15", "#FAB733", "#ACB334", "#69B34C"
)


names(colours_scores) <- c("E", "D", "C", "B", "A", "A+")

score_bar <- ggplot(
  scores, 
  aes(
    x = category, 
    y = score_delta, 
    fill = factor(score_symbol, levels = rev(scores$score_symbol))
    )
  ) +
  geom_bar(stat = "identity") +
  geom_point(
    data = data_fut %>%
      mutate(category = "score"), 
    aes(color = entity_type, y = score, fill = "black", shape = entity_type, size = 1.5),
    position=position_dodge(width=0.2)
    ) +
  scale_y_continuous(
    breaks = scores$score_label, 
    labels = scores$score_symbol,
    expand = expansion(mult = c(0,0.1))
    ) +
  scale_colour_2dii(colour_groups = data_fut$entity_type) +
  scale_fill_manual(values = colours_scores) +
  scale_shape_manual(
    values = c(16, 16, 1),
    labels = r2dii.plot::to_title(levels(data_fut$entity_type))
    ) +
  theme_2dii(
    base_size = 20
  ) + 
  theme(
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none"
    ) +
  labs(
    y = "Alignment score"
  )
score_bar
#ggsave("../../../visualisation/Executive-summary/score_bar.svg")

p <- ggplot(
  data_fut, 
  aes(
    x = tech_mix_green, 
    y = score, 
    color = entity_type, 
    shape = entity_type, 
    size = 1.5)
  ) +
  geom_point() + 
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "#C0C0C0") +
  geom_hline(yintercept = scores$score_upper[2], linetype = "dashed", color = "#C0C0C0") +
  scale_y_continuous(
    limits = c(1, 100),
    breaks = scores$score_label, 
    labels = scores$score_symbol,
    expand = expansion(mult = c(0,0.1))
    ) +
  scale_x_continuous(
    labels = scales::percent, 
    limits = c(0, 1), 
    expand = expansion(mult = c(0,0.1))
    ) +
  scale_shape_manual(
    values = c(16, 16, 1),
    labels = r2dii.plot::to_title(levels(data_fut$entity_type))
    ) +
  scale_colour_2dii(colour_groups = data_fut$entity_type) +
  theme_2dii(
    base_size = 20
  ) +
  theme(
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  guides(shape = "none", size = "none")
p

tech_mix <- tibble::tibble(
  tech_mix_green = seq(from = 0.05, to = 1, by = 0.05),
  category = "techmix"
)

tech_mix_bar_fut <- ggplot(
  tech_mix, 
  aes(x = category, y = tech_mix_green, fill = tech_mix_green)
  ) +
  geom_bar(stat = "identity", position = "fill") + 
  geom_point(
    data = data_fut %>%
      mutate(category = "techmix"), 
    aes(color = entity_type, shape = entity_type, size = 1.5),
    position=position_dodge(width=0.2)
    ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent, 
    expand = expansion(mult = c(0,0.1))
    ) +
  scale_fill_gradient(low = "#986B41", high = "#7bc17e") +
  scale_colour_2dii(colour_groups = data_fut$entity_type) +
  scale_shape_manual(
    values = c(16, 16, 1), 
    labels = r2dii.plot::to_title(levels(data_fut$entity_type))
    ) +
  theme_2dii(
    base_size = 20
  ) + 
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
    ) +
  labs(y = "Share of low-carbon\n technologies in Power sector")
tech_mix_bar_fut

score_bar + p + guide_area() + tech_mix_bar_fut + 
  plot_layout(ncol = 2, widths = c(0.7, 3.8), heights = c(3.8, 1), guides = "collect") +
  theme(
    legend.position = "left",
    legend.title = element_text()
    ) +
  guides(
    size = "none", 
    shape = "none",
    fill = "none",
    colour = guide_legend(title = "Entity type", override.aes = list(size = 4, shape = c(16, 16, 1)))
    )
#ggsave("../../../visualisation/Executive-summary/scatterplot_exposure_vs_alignment2.png")

