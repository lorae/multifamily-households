# Clustering analysis
# Goal: Cluster household data to produce archetypal households
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")
library("purrr")
library("tidyr")
library("forcats")

devtools::load_all("../demographr")

# Retrieve best fit model
k_fit <- readRDS("throughput/k6-best-model.rds")

# 1. Cluster centers (in scaled units)
round(k_fit$centers, 2)

# 2. Cluster sizes
k_fit$size

# 3. Total within-cluster sum of squares
k_fit$tot.withinss

# 4. Between-cluster sum of squares ratio (a quick fit metric)
k_fit$betweenss / k_fit$totss


cluster_counts_year <- cluster_counts_year |>
  mutate(cluster_label = fct_recode(factor(cluster), !!!cluster_labels))

hh_clustered <- hh_clustered |>
  mutate(cluster_label = fct_recode(factor(cluster), !!!cluster_labels))

cluster_labels <- c(
  "1" = "Adult Child + Parent",
  "2" = "Extended Family",
  "3" = "Couples",
  "4" = "Grandfamily",
  "5" = "Single-person",
  "6" = "Many Children"
)

cluster_counts_year <- cluster_counts_year |>
  mutate(cluster_label = recode(factor(cluster), !!!cluster_labels))


library(ggplot2)

ggplot(cluster_counts_year, aes(x = YEAR, y = weighted_count, color = cluster_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set2", name = "Household Type") +
  scale_x_continuous(breaks = seq(1900, 2020, by = 10)) +
  labs(
    title = "Household Archetypes Over Time",
    x = "Year",
    y = "Weighted Household Count"
  ) +
  theme_minimal(base_size = 14)

hh_archetype_percent <- crosstab_percent(
  data = hh_clustered,
  wt_col = "HHWT",
  group_by = c("YEAR", "cluster"),
  percent_group_by = "YEAR"
)

hh_archetype_percent <- hh_archetype_percent |>
  mutate(cluster_label = recode(factor(cluster), !!!cluster_labels))


hh_archetype_percent_scaled <- hh_archetype_percent |>
  group_by(cluster) |>
  mutate(
    baseline_2023 = percent[YEAR == 2023],
    scaled_count = percent / baseline_2023
  ) |>
  ungroup() 

ggplot(hh_archetype_percent_scaled, aes(x = YEAR, y = scaled_count, color = cluster_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set2", name = "Household Type") +
  scale_x_continuous(breaks = seq(1900, 2023, by = 10)) +
  labs(
    title = "Relative Growth of Household Archetypes (Indexed to 2023 = 1)",
    x = "Year",
    y = "Multiple of 2023 Household Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

# Calculate mean, 5th and 95th percentile within each cluster for each var
library(dplyr)
library(Hmisc)  # for wtd.quantile

summary_by_cluster <- hh_clustered |>
  group_by(cluster) |>
  summarise(
    across(
      .cols = c(n_spouse, n_child, n_parent, n_grandchild, n_other_rel, n_non_rel),
      .fns = list(
        mean = ~weighted.mean(.x, HHWT, na.rm = TRUE),
        p5   = ~wtd.quantile(.x, weights = HHWT, probs = 0.05, na.rm = TRUE),
        p95  = ~wtd.quantile(.x, weights = HHWT, probs = 0.95, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

# Z score by clutser 
# Attach z-scores if not done already
z_scores <- as_tibble(ipums_household_scaled)
colnames(z_scores) <- paste0(vars, "_z")

hh_clustered_with_z <- hh_clustered |>
  bind_cols(z_scores)

# Summarize mean z-score per variable within each cluster
cluster_z_profile <- hh_clustered_with_z |>
  group_by(cluster) |>
  summarise(
    across(
      .cols = ends_with("_z"),
      .fns = ~mean(.x, na.rm = TRUE),  # unweighted, because z-scores already standardized
      .names = "mean_{.col}"
    )
  )
