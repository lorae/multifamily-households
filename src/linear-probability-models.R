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

# Assume ipums_person is a tbl_dbi (DuckDB connection)
years <- ipums_person |> distinct(YEAR) |> collect() |> pull(YEAR)

results <- map_dfr(years, function(y) {
  dat <- ipums_person |>
    filter(YEAR == y) |>
    select(is_multifam, race_eth, PERWT) |>
    collect() |>
    filter(!is.na(is_multifam), !is.na(race_eth), !is.na(PERWT)) |>
    mutate(race_eth = factor(race_eth))  # 👈 force factor
  
  baseline <- levels(dat$race_eth)[1]
  message("Baseline for ", y, ": ", baseline)
  
  fit <- lm(is_multifam ~ -1 + race_eth, data = dat, weights = PERWT)
  broom::tidy(fit) |> mutate(YEAR = y, baseline = baseline)
})

results_filtered <- results |> 
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

# Plot
ggplot(results_filtered, aes(x = YEAR, y = estimate, color = term)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Probability of Multifamily Living Over Time, by Race/Ethnicity",
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
