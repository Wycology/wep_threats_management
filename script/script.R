# About the script -------------------------------------------------------------------------------
# We include codes used to generate plots and results in the manuscript titled:
# Threats and management options of wild edible plants in semi-arid lands of northwestern Kenya.
# Submitted to Journal of Ecology and Society. 
# Full details on the manuscript will be given once published, including citation and doi.

# R and R Studio ---------------------------------------------------------------------------------

# R version 4.2.1 (2022-06-23 ucrt)
# RStudio 2022.07.1+554

# Libraries --------------------------------------------------------------------------------------

library(tidyverse)  # Version 1.3.2
library(rstatix)    # Version 0.7.0
library(ggpubr)     # Version 0.4.0
library(ggrepel)    # Version 0.9.1

# Loading in datasets ----------------------------------------------------------------------------

fgd_threats <- read_csv('data/fgd_threats.csv') # Threat scores from focus group discussions.

# site == The three study community units, Nasiger, Atala Kamusio, and Lopur in Turkana County.
# informant == Unique code identifying the FGD participant. NAS1 means 1st participant from Nasiger.
# threat== Threat categories. See supplementary Tables 1 and 2 in the manuscript.
# rank_threat == Score assigned to the threat category by focus group discussion participant. 

field_threats <- read_csv('data/field_threats.csv') # Threat scores from field plot surveys.

# plot_id == Unique identifier of survey plot. NAS_1, plot one at Nasiger.
# site == The three study community units, Nasiger, Atala Kamusio, and Lopur in Turkana County.
# threat == Threat categories. See supplementary Tables 1 and 2 in the manuscript.
# rank_threat == Score assigned to the threat category by three field observers.

fgd_management <- read_csv('data/fgd_management.csv') # Management options scores from focus group discussions.

# site == The three study community units, Nasiger, Atala Kamusio, and Lopur in Turkana County.
# informant == Unique code identifying the FGD participant. NAS1 means 1st participant from Nasiger.
# management== Management option categories. See supplementary Table 3 in the manuscript.
# potential_rank == Score assigned to the management option category by focus group discussion participant. 

# Table 1: Ranks of threat scores from FGD per community unit and all community units combined. 

fgd_threats |>
  group_by(site, threat) |>
  summarise(score = sum(rank_threat)) |>
  spread(key = site, value = score) |>
  mutate(All_communities = rowSums(across(where(is.numeric)))) |>
  arrange(-All_communities) |>
  mutate(
    nas_rank = rank(-Nasiger),
    ata_rank = rank(-`Atala Kamusio`),
    lop_rank = rank(-Lopur),
    all_rank = rank(-All_communities)
  ) |>
  select(
    threat,
    Nasiger,
    nas_rank,
    `Atala Kamusio`,
    ata_rank,
    Lopur,
    lop_rank,
    All_communities,
    all_rank
  ) |>
  knitr::kable()

# Table 2: Threat scores and corresponding ranks from field plot surveys within the three study communities.

field_threats |>
  group_by(site, threat) |>
  summarise(score = sum(rank_threat)) |>
  spread(key = site, value = score) |>
  mutate(All_communities = rowSums(across(where(is.numeric)))) |>
  arrange(-All_communities) |>
  mutate(
    nas_rank = rank(-Nasiger),
    ata_rank = rank(-`Atala Kamusio`),
    lop_rank = rank(-Lopur),
    all_rank = rank(-All_communities)
  ) |>
  select(
    threat,
    Nasiger,
    nas_rank,
    `Atala Kamusio`,
    ata_rank,
    Lopur,
    lop_rank,
    All_communities,
    all_rank
  ) |>
  knitr::kable()

# Table 3: Management options scores and corresponding ranks from FGDs within the three study communities.

fgd_management |>
  group_by(site, management) |>
  summarise(score = sum(potential_rank)) |>
  spread(key = site, value = score) |>
  mutate(All_communities = rowSums(across(where(is.numeric)))) |>
  arrange(-All_communities) |>
  mutate(
    nas_rank = floor(rank(-Nasiger)),
    ata_rank = floor(rank(-`Atala Kamusio`)),
    lop_rank = floor(rank(-Lopur)),
    all_rank = floor(rank(-All_communities))
  ) |>
  select(
    management,
    Nasiger,
    nas_rank,
    `Atala Kamusio`,
    ata_rank,
    Lopur,
    lop_rank,
    All_communities,
    all_rank
  ) |>
  knitr::kable()

# Figure 2: FGD threat scores comparison across Nasiger, Atala Kamusio, and Lopur community units

fgd_agr <- # Aggregating the threat scores for pairwise mean comparison
  fgd_threats |> group_by(threat) |> mutate(site_id = rep(1:14, 3)) |>
  select(site_id, site, rank_threat) |> convert_as_factor(site_id, site) |>
  reorder_levels(site, order = c('Nasiger', 'Atala Kamusio', 'Lopur'))

pwc <- fgd_agr |> # Building pairwise mean comparison object called pwc.
  wilcox_test(rank_threat ~ site,
              paired = FALSE,
              p.adjust.method = "bonferroni") |> # Bonferroni is appropriate as it supports discrete data in our case.
  add_xy_position(x = "site")

png( # Settings for the plot.
  filename = 'output/Figure_2.png', 
  width = 2000,
  height = 1600,
  res = 300
)
ggboxplot(
  fgd_agr,
  x = "site",
  y = "rank_threat",
  lwd = 0.5,
  color = 'black',
  font.label = list(size = 9, color = "black")
) +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  stat_pvalue_manual(pwc, hide.ns = FALSE, label = "{p.adj}{p.adj.signif}", size = 2.5) +
  labs(y = "Threat Score", x = 'Community Unit') +
  facet_wrap(
    ~ factor(
      threat,
      levels = c(
        "Climate change",
        "Invasive species",
        "Overstocking/overgrazing",
        "Selective harvesting/Overharvesting",
        "Fuelwood collection/charcoal burning",
        "Agricultural expansion",
        "Uncontrolled fire",
        "Infrastructural development",
        "Pests and diseases",
        "Others"
      )
    ),
    nrow = 2,
    ncol = 5,
    strip.position = "left"
  ) +
  theme_classic() +
  theme(text = element_text(size = 8, color = 'black', face = 'bold')) +
  theme(axis.text.x = element_text(
    angle = 90,
    face = 'bold', 
    color = 'black'
  )) +
  theme(axis.text.y = element_text(color = 'black')) 
dev.off()

# Figure 3: Field threat scores comparison across Nasiger, Atala Kamusio, and Lopur community units

field_agr <- # Aggregating the scores for pairwise mean comparison
  field_threats |> group_by(threat) |> mutate(plot_id = rep(1:80, 3)) |>
  select(plot_id, site, rank_threat) |> convert_as_factor(plot_id, site) |>
  reorder_levels(site, order = c('Nasiger', 'Atala Kamusio', 'Lopur'))

pwc <- field_agr |># Building pairwise mean comparison object called pwc.
  wilcox_test(rank_threat ~ site ,
              paired = FALSE,
              p.adjust.method = "bonferroni") |>
  add_xy_position(x = "site")

png(
  filename = 'output/Figure_3.png',
  width = 2000,
  height = 1600,
  res = 300
)
ggboxplot(
  field_agr,
  x = "site",
  y = "rank_threat",
  lwd = 0.5,
  color = 'black',
  font.label = list(size = 9, color = 'black')
) +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  stat_pvalue_manual(pwc, hide.ns = FALSE, label = "{p.adj}{p.adj.signif}", size = 2.5) +
  labs(y = "Score", x = 'Community Unit') +
  facet_wrap(
    ~ factor(
      threat,
      levels = c(
        'Overstocking/overgrazing',
        'Selective harvesting/Overharvesting',
        'Invasive species',
        'Fuelwood collection/charcoal burning',
        'Agricultural expansion',
        'Uncontrolled fire',
        'Infrastructural development',
        'Pests and diseases',
        'Others'
      )
    ),
    nrow = 2,
    ncol = 5,
    strip.position = 'left'
  ) +
  theme_classic() +
  theme(text = element_text(size = 10, color = 'black', face = 'bold')) +
  theme(axis.text.x = element_text(
    angle = 90,
    face = 'bold',
    color = 'black'
  )) +
  theme(axis.text.y = element_text(color = 'black')) +
  theme(strip.text.x = element_text(size = 10))
dev.off()

# Figure 5: Correlation between FGD and field threat ranks across the study communities 

df_fgd <- fgd_threats |>
  filter(threat != 'Climate change') |> # Dropping the Climate change threat as we did not have it in field data
  group_by(site, threat) |>
  summarise(score = sum(rank_threat)) |>
  ungroup() |>
  spread(key = site, value = score) |>
  mutate(All_communities = rowSums(across(where(is.numeric)))) |>
  arrange(-All_communities) |>
  mutate(nas_rank = rank(-Nasiger),
         ata_rank = rank(-`Atala Kamusio`),
         lop_rank = rank(-Lopur),
         all_rank = rank(-All_communities)) |>
  select(threat, nas_rank, ata_rank,
         lop_rank, all_rank) |>
  mutate(study = 'fgd') |>
  gather(key = key, value = value, -threat, -study)

df_field <- field_threats |>
  group_by(site, threat) |>
  summarise(score = sum(rank_threat)) |>
  ungroup() |>
  spread(key = site, value = score) |>
  mutate(All_communities = rowSums(across(where(is.numeric)))) |>
  arrange(-All_communities) |>
  mutate(nas_rank = rank(-Nasiger),
         ata_rank = rank(-`Atala Kamusio`),
         lop_rank = rank(-Lopur),
         all_rank = rank(-All_communities)) |>
  select(threat, nas_rank, ata_rank, lop_rank, all_rank) |>
  mutate(study = 'field') |>
  gather(key = key, value = value_field, -threat, -study) |>
  mutate(study_field = study) |> select(-study) 

png(
  filename = 'output/Figure_5.png',
  width = 2000,
  height = 2000,
  res = 300
)
inner_join(df_fgd, df_field, by = c('threat', 'key')) |>
  mutate(
    label = case_when(
      key == 'all_rank' &
        value == 1 &
        value_field == 3 ~ 'Invasive species',
      key == 'all_rank' &
        value == 2 &
        value_field == 1 ~ 'Overstocking/overgrazing',
      key == 'all_rank' &
        value == 3 &
        value_field == 2 ~ 'Selective harvesting/overharvesting',
      key == 'ata_rank' &
        value == 5 &
        value_field == 8 ~ 'Infrastructural development',
      key == 'lop_rank' &
        value == 2 &
        value_field == 5 ~ 'Selective harvesting/overharvesting',
      key == 'lop_rank' &
        value == 6 &
        value_field == 2 ~ 'Invasive species',
      key == 'nas_rank' &
        value == 5 &
        value_field == 7 ~ 'Uncontrolled fire',
      key == 'nas_rank' &
        value == 7 &
        value_field == 5 ~ 'Pests and diseases'
    )
  ) |>
  mutate(
    name = case_when(
      key == 'nas_rank' ~ 'Nasiger',
      key == 'ata_rank' ~ 'Atala Kamusio',
      key == 'lop_rank' ~ 'Lopur',
      key == 'all_rank' ~ 'All Communities'
    )
  ) |>
  ggplot(aes(value, value_field)) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = label)) +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  geom_smooth(method = 'lm', col = 'black') +
  stat_cor(method = 'pearson', label.x = 2, label.y = 10 ) +
  facet_wrap( ~ factor(
    name,
    levels = c('Nasiger', 'Atala Kamusio', 'Lopur', 'All Communities')
  )) +
  theme_classic() +
  theme(axis.text.x = element_text(color = 'black')) +
  theme(axis.text.y = element_text(color = 'black')) +
  labs(x = 'Focus Group Discussion Scores',
       y = 'Field Observation Scores') +
  theme(text = element_text(size = 14, face = 'bold'))
dev.off()

# Figure 6: Compare management options for the WEPs across the communities

fgd_agr <- # Aggregating the management scores for pairwise mean comparison
  fgd_management |> group_by(management) |> mutate(fgd_id = rep(1:14, 3)) |>
  select(fgd_id, site, potential_rank) |> convert_as_factor(fgd_id, site) |>
  reorder_levels(site, order = c('Nasiger', 'Atala Kamusio', 'Lopur'))

pwc <- fgd_agr |>
  wilcox_test(potential_rank ~ site,
              paired = FALSE,
              p.adjust.method = "bonferroni") |>
  add_xy_position(x = "site")

png(
  filename = 'output/Figure_6.png',
  width = 2000,
  height = 1800,
  res = 300
)
ggboxplot(
  fgd_agr,
  x = "site",
  y = "potential_rank",
  lwd = 0.4,
  color = 'black',
  font.label = list(size = 10, color = 'black')
) +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  stat_pvalue_manual(pwc, hide.ns = FALSE, label = "{p.adj}{p.adj.signif}", size = 2) +
  labs(y = "Score", x = 'Community Unit') +
  facet_wrap(
    ~ factor(
      management,
      levels = c(
        'Mitigate climate change',
        'Preserve local knowledge about the WEPs',
        'Selection, propagation, processing and marketing',
        'Cultivate WEPs',
        'Control harvesting for food and fodder',
        'Prohibit charcoal burning',
        'Assess nutrition and toxicity',
        'Create public awareness on WEPs',
        'Conserve in sacred areas',
        'Establish protected areas',
        'Monitor and inventor',
        'Others'
      )
    ),
    nrow = 2,
    ncol = 6,
    strip.position = 'left'
  ) +
  theme_classic() +
  theme(text = element_text(size = 9, color = 'black', face = 'bold')) +
  theme(axis.text.x = element_text(
    angle = 90,
    face = 'bold',
    color = 'black'
  )) +
  theme(axis.text.y = element_text(color = 'black', face = 'bold')) 
dev.off()