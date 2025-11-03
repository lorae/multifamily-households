# linear-probability-models
# 
# Plot average person-level household size over the decades in aggregate.
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("scales")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

library(dplyr)
library(purrr)
library(broom)

# ----- Step 1: Define functions ----- #
# Step A: Run a linear probability model each year
run_lpm_by_year <- function(tbl = ipums_person, controls = NULL, outcome = "is_multifam") {
  years <- tbl |> distinct(YEAR) |> collect() |> pull(YEAR)
  ipums_person <- tbl(con, "ipums_person")
  
  map_dfr(years, function(y) {
    dat <- ipums_person |>
      filter(YEAR == y) |>
      select(all_of(c(outcome, "race_eth", "PERWT", controls))) |>
      collect() |>
      filter(!is.na(!!sym(outcome)), !is.na(race_eth), !is.na(PERWT)) |>
      mutate(race_eth = factor(race_eth))
    
    baseline <- levels(dat$race_eth)[1]
    message("Baseline for ", y, ": ", baseline)
    
    # Build model formula dynamically
    fml <- as.formula(
      paste(outcome, "~ -1 + race_eth", 
            if (!is.null(controls)) paste("+", paste(controls, collapse = " + ")) else "")
    )
    
    fit <- lm(fml, data = dat, weights = PERWT)
    
    broom::tidy(fit) |> mutate(YEAR = y, baseline = baseline)
  })
}

# Step B: Clean the output to include results by 5 race/eth groups
filter_by_race <- function(result) {
  result |>   
    filter(term %in% c("race_ethAAPI", "race_ethAIAN", "race_ethBlack", "race_ethHispanic", "race_ethWhite")) |>
    arrange(term, YEAR) |>
    mutate(term = recode(term,
                         "race_ethAAPI" = "AAPI",
                         "race_ethAIAN" = "AIAN",
                         "race_ethBlack" = "Black",
                         "race_ethHispanic" = "Hispanic",
                         "race_ethWhite" = "White"
    )) |>
    mutate(term = factor(term, levels = c("Hispanic", "AIAN", "AAPI", "Black", "White"))) # This orders labels in legend
}

# Step C: graph the results
plot_race_trends <- function(results, title) {
  results |>
    ggplot(aes(x = YEAR, y = estimate, color = term)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +
    theme_minimal(base_size = 14) +
    labs(
      title = title,
      x = "Year",
      y = "LPM Estimate: Probability of Multifamily Living",
      color = "Term"
    ) +
    scale_color_manual(values = c(
      "Hispanic" = "#acfa70",
      "AIAN" = "#00cf97",
      "AAPI" = "#0097a3",
      "Black" = "#006290",
      "White" = "#292f56"
    )) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "bottom"
    )
}

# ----- Step 2: Produce results ----- #
# Base model
base_model_plot <- run_lpm_by_year() |>
  filter_by_race() |>
  plot_race_trends(title = "Probability of multifamily living over time, by race/ethnicity")

base_model_plot

# Basic demographics model: age, sex
basic_demo_plot <- run_lpm_by_year(controls = c("age_bucket", "SEX")) |>
  filter_by_race() |>
  plot_race_trends(title = "Probability of multifamily living over time, by race/ethnicity \nwith age and sex controls")

basic_demo_plot

# SES inputs: income, education

# Number of people in own subfamily

# Geography

