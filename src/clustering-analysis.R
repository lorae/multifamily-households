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

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_household <- tbl(con, "ipums_household")

set.seed(123)

# Prepare data for k-means: select variables and scale
# Variables to process
vars <- c(
  "n_spouse",
  "n_child",
  "n_parent",
  "n_grandchild",
  "n_other_rel",
  "n_non_rel"
)


ipums_household_tb <- ipums_household |>
  filter(GQ %in% c(0,1,2)) |>
  select(YEAR, HHWT, all_of(vars)) |>
  collect() 

ipums_household_scaled <-  scale(ipums_household_tb |> select(-YEAR, -HHWT))


# ----- elbow
# Prepare a list to store all models
kmeans_results <- list()

# Loop over k values
for (k in 1:15) {
  message("Running k-means for k = ", k, " ...")
  km <- kmeans(ipums_household_tb, centers = k, nstart = 10)
  kmeans_results[[as.character(k)]] <- km
  message("Completed k = ", k)
}

# Extract within-cluster SS (for elbow plot)
wss <- sapply(kmeans_results, function(x) x$tot.withinss)

# Plot the elbow
plot(1:15, wss, type = "b",
     xlab = "Number of clusters (k)",
     ylab = "Total within-cluster SS",
     main = "Elbow Method")

k_fit <- kmeans_results[["6"]]  # retrieve model for k = 6

# 1. Cluster centers (in scaled units)
round(k_fit$centers, 2)

# 2. Cluster sizes
k_fit$size

# 3. Total within-cluster sum of squares
k_fit$tot.withinss

# 4. Between-cluster sum of squares ratio (a quick fit metric)
k_fit$betweenss / k_fit$totss


hh_clustered <- ipums_household_tb |>
  mutate(cluster = k_fit$cluster)

cluster_counts_year <- crosstab_count(
  data = hh_clustered,
  wt_col = "HHWT",
  group_by = c("YEAR", "cluster")
)

cluster_labels <- c(
  "1" = "Couple + Kids",
  "2" = "Grandfamily",
  "3" = "Roommates",
  "4" = "Extended Family",
  "5" = "Single-person",
  "6" = "Adult Child + Parent"
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

cluster_counts_scaled <- cluster_counts_year |>
  group_by(cluster_label) |>
  mutate(
    baseline_2023 = weighted_count[YEAR == 2023],
    scaled_count = weighted_count / baseline_2023
  ) |>
  ungroup()

ggplot(cluster_counts_scaled, aes(x = YEAR, y = scaled_count, color = cluster_label)) +
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

