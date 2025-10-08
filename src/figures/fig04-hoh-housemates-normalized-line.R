# fig04-hoh_housemates_normalized_line
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