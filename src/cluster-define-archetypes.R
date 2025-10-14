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
k_data <- readRDS("throughput/k6-best-model-cluster-labels.rds")

# ----- Step 1: get z-score cluster centers ----- #
# Cluster centers (z scores)
cluster_centers <- round(k_fit$centers, 2)

# ----- Step 2: Get mean cluster centers ----- #
# in units of persons

vars <- c("n_spouse", "n_child", "n_parent", "n_grandchild", "n_other_rel", "n_non_rel")

get_cluster_mean <- function(data, var_name) {
  crosstab_mean(
    data,
    value = var_name,
    wt_col = "HHWT",
    group_by = "cluster"
  ) |>
    select(cluster, weighted_mean) |>
    rename(!!var_name := weighted_mean)
}

cluster_means <- vars |>
  map(~ get_cluster_mean(k_data, .x)) |>
  reduce(left_join, by = "cluster")
  
# ----- Step 3: Get 5th percentile and 95th percentile within each cluster ----- #
# Weighted quantile helper (internal)
weighted_quantile <- function(x, w, probs = c(0.05, 0.95)) {
  df <- tibble(x = x, w = w) |> filter(!is.na(x) & !is.na(w))
  df <- df |> arrange(x) |> mutate(w_cum = cumsum(w) / sum(w))
  sapply(probs, function(p) {
    idx <- which.min(abs(df$w_cum - p))
    df$x[idx]
  })
}

# Function to get weighted percentiles for a single variable
get_cluster_percentile <- function(data, var_name) {
  data |>
    group_by(cluster) |>
    summarise(
      p5  = weighted_quantile(.data[[var_name]], HHWT, probs = 0.05)[1],
      p95 = weighted_quantile(.data[[var_name]], HHWT, probs = 0.95)[1],
      .groups = "drop"
    ) |>
    rename(
      !!paste0(var_name, "_p5")  := p5,
      !!paste0(var_name, "_p95") := p95
    )
}

# Apply to all vars and merge into one wide table
cluster_percentile_list <- vars |>
  map(~ get_cluster_percentile(k_data, .x))


# ----- Step 4: Export these tables ----- #

# Export z-scores (cluster centers)
cluster_centers_df <- as.data.frame(cluster_centers)
cluster_centers_df <- cluster_centers_df |>
  mutate(cluster = row_number(), .before = 1)

write_csv(cluster_centers_df, "output/tables/tab01-cluster-zscores.csv")

# Export means
write_csv(cluster_means, "output/tables/tab02-cluster-means.csv")

# Export 90% intervals (5th to 95th percentile)
# Merge all percentile tables
cluster_percentiles <- cluster_percentile_list |>
  reduce(left_join, by = "cluster")

# Reshape to show (low, high) format for each variable
cluster_intervals <- cluster_percentiles |>
  mutate(cluster = cluster, .before = 1)

# Create a version with formatted intervals
cluster_intervals_formatted <- tibble(cluster = cluster_percentiles$cluster)

for (var in vars) {
  p5_col <- paste0(var, "_p5")
  p95_col <- paste0(var, "_p95")
  
  cluster_intervals_formatted <- cluster_intervals_formatted |>
    mutate(
      !!var := paste0("(", cluster_percentiles[[p5_col]], ", ", 
                      cluster_percentiles[[p95_col]], ")")
    )
}

write_csv(cluster_intervals_formatted, "output/tables/tab03-cluster-90pct-intervals.csv")

# Cluster sizes
cluster_sizes <- k_fit$size

# ----- Step 4: Get cluster counts by years ----- #
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
