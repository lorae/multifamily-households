# fig02-household-members
# 
# Understand the configuration of a household 
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

# Helper function to compute weighted mean by YEAR and rename column
get_component_mean <- function(var) {
  crosstab_mean(
    data = ipums_household |> filter(GQ %in% c(0,1,2)),
    value = var,
    wt_col = "HHWT",
    group_by = c("YEAR")
  ) |>
    select(YEAR, weighted_mean) |>
    rename(!!var := weighted_mean)
}

# Variables to process
vars <- c(
  "n_spouse",
  "n_child",
  "n_parent",
  "n_grandchild",
  "n_other_rel",
  "n_non_rel"
)

# Apply the function to each variable and reduce with left_join
hh_components <- vars |>
  map(get_component_mean) |>
  reduce(left_join, by = "YEAR")


# Step 1: Add reference person column
hh_long <- hh_components |>
  mutate(reference_person = 1) |>
  pivot_longer(
    cols = -YEAR,
    names_to = "component",
    values_to = "value"
  )

# Step 2: Optional: reorder components for nicer stacking
# (reference_person at bottom, children/spouse next, etc.)
hh_long <- hh_long |>
  mutate(component = factor(
    component,
    levels = c(
      "n_non_rel",
      "n_other_rel",
      "n_parent",
      "n_grandchild",
      "n_child",
      "n_spouse",
      "reference_person"
    ),
    labels = c(
      "Non-relatives",
      "Other relatives",
      "Parents / in-laws",
      "Grandchildren",
      "Children",
      "Spouse",
      "Reference person"
    )
  ))

# Step 3: Plot sand chart
ggplot(hh_long, aes(x = YEAR, y = value, fill = component)) +
  geom_area(color = "white", size = 0.2, alpha = 0.9) +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Household Composition Over Time",
    x = NULL,
    y = "Average number of people in household",
    fill = "Component"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


# ----- Step 4: Scale components relative to 2023 (current) value)

hh_components_scaled <- hh_components |>
  mutate(across(
    n_spouse:n_non_rel,
    ~ .x / .x[which(YEAR == 2023)]
  ))


hh_components_long <- hh_components_scaled |>
  pivot_longer(
    cols = n_spouse:n_non_rel,
    names_to = "component",
    values_to = "relative_value"
  )

# Optional: prettier labels for legend
component_labels <- c(
  n_spouse = "Spouse",
  n_child = "Child",
  n_parent = "Parent",
  n_grandchild = "Grandchild",
  n_other_rel = "Other relative",
  n_non_rel = "Non-relative"
)

# ----- Plot ----- #
fig_components <- hh_components_long |>
  ggplot(aes(x = YEAR, y = relative_value, color = component)) +
  geom_line(size = 1.2) +
  scale_color_brewer(palette = "Dark2", labels = component_labels) +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  labs(
    title = "Household Members per Household (Normalized to 2023 = 1)",
    x = NULL,
    y = "Relative to 2023"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black")
  )

fig_components

# ---- k means
data1900 <- ipums_household |> 
  filter(YEAR == 1900) |> 
  select(n_spouse, n_child, n_parent, n_grandchild, n_other_rel, n_non_rel) |>
  collect()

# Suppose hh_data has columns: n_spouse, n_child, n_parent, n_grandchild, n_other_rel, n_non_rel
data1900_scaled <- data1900 %>% 
  select(n_spouse:n_non_rel) %>% 
  scale()

set.seed(123)
k_fit <- kmeans(data1900_scaled, centers = 6)  # try different k

data1900$cluster <- k_fit$cluster

# Look at cluster centers (these are your archetypes)
round(k_fit$centers, 2)

centers_unscaled <- sweep(k_fit$centers, 2, attr(data1900_scaled, "scaled:scale"), "*")
centers_unscaled <- sweep(centers_unscaled, 2, attr(data1900_scaled, "scaled:center"), "+")
round(centers_unscaled, 2)

table(data1900$cluster)

# ----- elbow
wss <- sapply(1:15, function(k) {
  kmeans(data1900_scaled, centers = k, nstart = 10)$tot.withinss
})

plot(1:15, wss, type = "b",
     xlab = "Number of clusters (k)",
     ylab = "Total within-cluster SS",
     main = "Elbow Method")

# ----- silhouette
library(cluster)

set.seed(123)
sample_idx <- sample(nrow(data1900_scaled), 20000)
data_sample <- data1900_scaled[sample_idx, ]

sil_width <- sapply(2:15, function(k) {
  km <- kmeans(data_sample, centers = k, nstart = 10)
  ss <- silhouette(km$cluster, dist(data_sample))
  mean(ss[, 3])
})

plot(2:15, sil_width, type = "b",
     xlab = "Number of clusters (k)",
     ylab = "Average silhouette width (subsample)",
     main = "Silhouette Method (Subsample)")

# ---- k means with 8 types
data1900 <- ipums_household |> 
  filter(YEAR == 1900) |> 
  select(n_spouse, n_child, n_parent, n_grandchild, n_other_rel, n_non_rel) |>
  collect()

# Suppose hh_data has columns: n_spouse, n_child, n_parent, n_grandchild, n_other_rel, n_non_rel
data1900_scaled <- data1900 %>% 
  select(n_spouse:n_non_rel) %>% 
  scale()

set.seed(123)
k_fit <- kmeans(data1900_scaled, centers = 8)  # try different k

data1900$cluster <- k_fit$cluster

# Look at cluster centers (these are your archetypes)
round(k_fit$centers, 2)

centers_unscaled <- sweep(k_fit$centers, 2, attr(data1900_scaled, "scaled:scale"), "*")
centers_unscaled <- sweep(centers_unscaled, 2, attr(data1900_scaled, "scaled:center"), "+")
round(centers_unscaled, 2)

table(data1900$cluster)

# ---- k means with 5 types
data1900 <- ipums_household |> 
  filter(YEAR == 1900) |> 
  select(n_spouse, n_child, n_parent, n_grandchild, n_other_rel, n_non_rel) |>
  collect()

# Suppose hh_data has columns: n_spouse, n_child, n_parent, n_grandchild, n_other_rel, n_non_rel
data1900_scaled <- data1900 %>% 
  select(n_spouse:n_non_rel) %>% 
  scale()

set.seed(123)
k_fit <- kmeans(data1900_scaled, centers = 5)  # try different k

data1900$cluster <- k_fit$cluster

# Look at cluster centers (these are your archetypes)
round(k_fit$centers, 2)

centers_unscaled <- sweep(k_fit$centers, 2, attr(data1900_scaled, "scaled:scale"), "*")
centers_unscaled <- sweep(centers_unscaled, 2, attr(data1900_scaled, "scaled:center"), "+")
round(centers_unscaled, 2)

table(data1900$cluster)

# ---- k means with 7 types
data <- ipums_household |> 
  filter(YEAR == 2023) |> 
  select(n_spouse, n_child, n_parent, n_grandchild, n_other_rel, n_non_rel) |>
  collect()

# Suppose hh_data has columns: n_spouse, n_child, n_parent, n_grandchild, n_other_rel, n_non_rel
data_scaled <- data %>% 
  select(n_spouse:n_non_rel) %>% 
  scale()

set.seed(123)
k_fit <- kmeans(data_scaled, centers = 8)  # try different k

data$cluster <- k_fit$cluster

# Look at cluster centers (these are your archetypes)
round(k_fit$centers, 2)

centers_unscaled <- sweep(k_fit$centers, 2, attr(data_scaled, "scaled:scale"), "*")
centers_unscaled <- sweep(centers_unscaled, 2, attr(data_scaled, "scaled:center"), "+")
round(centers_unscaled, 2)

table(data$cluster)

# ----- elbow
wss <- sapply(1:15, function(k) {
  kmeans(data_scaled, centers = k, nstart = 10)$tot.withinss
})

plot(1:15, wss, type = "b",
     xlab = "Number of clusters (k)",
     ylab = "Total within-cluster SS",
     main = "Elbow Method")

# --- PCA
data1900 <- ipums_household |> 
  filter(YEAR == 1900 & GQ %in% c(0,1,2)) |> 
  select(
    n_spouse, n_child, n_parent, n_grandchild, n_other_rel, n_non_rel,
    AGE,
    race_eth  # categorical with 7 categories
  ) |> 
  collect()