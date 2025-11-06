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
ipums_person <- tbl(con, "ipums_person_with_hh") |>
  filter(AGE >= 18 & AGE <= 65) |>
  filter(PERNUM == 1)

library(dplyr)
library(purrr)
library(broom)

# ----- Step 1: Define functions ----- #
# Step A: Run a linear probability model each year
run_lpm_by_year <- function(tbl = ipums_person, controls = NULL, outcome = "is_multifam") {
  years <- tbl |> distinct(YEAR) |> collect() |> pull(YEAR)

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
plot_race_trends <- function(results, title, ymin = NULL, ymax = NULL) {
  p <- results |>
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
  
  # Add y-axis limits only if at least one is specified
  if (!is.null(ymin) || !is.null(ymax)) {
    p <- p + coord_cartesian(ylim = c(ymin, ymax))
  }
  
  return(p)
}


# ----- Step 2: Produce results ----- #
# Base model
base_model_data <- run_lpm_by_year() |>
  filter_by_race() 
base_model_plot <- base_model_data |>
  plot_race_trends(title = "1: Probability of multifamily living over time, by race/ethnicity\nno controls",
                   ymin = 0.00, ymax = 0.25)

base_model_plot

# Basic demographics model: child
child_data <- run_lpm_by_year(controls = c("n_child")) |>
  filter_by_race() 
child_plot <- child_data |>
  plot_race_trends(title = "2: Probability of multifamily living over time, by race/ethnicity \nwith age and sex controls",
                   ymin = 0.00, ymax = 0.25)

child_plot


# Basic demographics model: age, sex
demo_data <- run_lpm_by_year(controls = c("n_child")) |>
  filter_by_race() 
demo_plot <- demo_data |>
  plot_race_trends(title = "2: Probability of multifamily living over time, by race/ethnicity \nwith age and sex controls",
                   ymin = 0.00, ymax = 0.25)

demo_plot

# SES inputs: income, education

# NOTE: This educ variable is not continuous like I'm implying by running this regression. It needs to be transformed
demo_ses_data <- run_lpm_by_year(controls = c("age_bucket", "SEX", "hhinc_harmonized", "EDUC")) |>
  filter_by_race() 
demo_ses_plot <- demo_ses_data |>
  plot_race_trends(title = "3: Probability of multifamily living over time, by race/ethnicity \nwith age, sex, and household income, and education controls",
                   ymin = 0.00, ymax = 0.25)

demo_ses_plot

# Number of people in own subfamily

# Geography

# outcomes: housing unit attributes such as kitchen, unitsstr, room, bedroom, ppbr
df <- ipums_person |>
  mutate(
    is_multifam = as.integer(is_multifam),
    n_child_group = if_else(n_child >= 8, 8L, n_child)  # lump 8+ together
  ) |>
  group_by(n_child_group) |>
  summarise(
    p_multifam = mean(is_multifam, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  collect()

ggplot(df, aes(x = factor(n_child_group), y = p_multifam)) +
  geom_point(size = 2) +
  geom_line(aes(group = 1)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Number of Children (8+ grouped)",
    y = "Percent Multifamily",
    title = "Likelihood of Living in Multifamily Household by Number of Children"
  ) +
  theme_minimal(base_size = 14)

# rent burden
ipums_person |>
  filter(OWNERSHP == 2, 
         !is.na(RENT), 
         !is.na(hhinc_harmonized),
         hhinc_harmonized > 0) |>
  mutate(rent_burden = (RENT * 13) / hhinc_harmonized) |>
  filter(rent_burden > 0, rent_burden < 2) |>
  pull(rent_burden) |>
  hist(
    breaks = 50,
    main = "Rent Burden Among Renters",
    xlab = "Annual Rent / Household Income",
    col = "skyblue",
    border = "white"
  )

# 
library(dplyr)
library(ggplot2)
library(scales)

df <- ipums_person |>
  filter(OWNERSHP == 2,                 # renters only
         !is.na(RENT),
         !is.na(hhinc_harmonized),
         hhinc_harmonized > 0) |>
  mutate(
    rent_burden = (RENT * 12) / hhinc_harmonized,
    is_multifam = as.integer(is_multifam),
    n_child_group = if_else(n_child >= 8, 8L, n_child)
  ) |>
  filter(rent_burden > 0, rent_burden < 2) |>
  group_by(n_child_group) |>
  summarise(
    mean_rent_burden = mean(rent_burden, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  collect()

ggplot(df, aes(x = factor(n_child_group), y = mean_rent_burden)) +
  geom_point(size = 2) +
  geom_line(aes(group = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Number of Children (8+ grouped)",
    y = "Average Rent Burden (as % of Income)",
    title = "Average Rent Burden by Number of Children, Renters Only"
  ) +
  theme_minimal(base_size = 14)

# Try 2023
df <- ipums_person |>
  filter(
    YEAR == 2023,
    OWNERSHP == 2, # renters only
    PERNUM == 1, # hoh
    !is.na(RENT),
    !is.na(hhinc_harmonized),
    hhinc_harmonized > 0
    ) |>
  mutate(
    rent_burden = (RENT * 12) / hhinc_harmonized,
    is_multifam = as.integer(is_multifam),
    n_child_group = if_else(n_child >= 8, 8L, n_child)
  )

 ####
library(dplyr)
library(ggplot2)
library(scales)

# ---- Step 1: Filter to low-income renter heads of household in 2023 ----
df_low_inc <- ipums_person |>
  filter(
    YEAR == 2023,
    OWNERSHP == 2,                # renters only
    n_child > 0, # at least one child living with them
    PERNUM == 1, #hoh
    !is.na(RENT),
    !is.na(hhinc_harmonized),
    hhinc_harmonized > 0
  ) |>
  mutate(
    rent_burden = (RENT * 12) / hhinc_harmonized
  ) |>
  filter(rent_burden > 0, rent_burden < 2) |>  # exclude impossible values
  collect() |>
  mutate(
    # Bin rent burden into 5% bands (0–4.9, 5–9.9, …)
    rent_burden_band = cut(
      rent_burden,
      breaks = seq(0, 2, by = 0.05),
      labels = sprintf("%g–%g",
                       seq(0, 1.95, by = 0.05) * 100,
                       seq(0.049, 2, by = 0.05) * 100),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# ---- Step 2: Compute weighted percent of multifamily households per band ----
df_low_inc_crosstab <- crosstab_percent(
  df_low_inc,
  wt_col = "PERWT",                       # or "HHWT" if you want household weighting
  group_by = c("rent_burden_band", "is_multifam", "CITY"),
  percent_group_by = c("is_multifam", "CITY")
) |>
  filter(is_multifam == 1, !is.na(rent_burden_band))  # drop the NA bin

# ---- Step 3: Plot ----
ggplot(
  df_low_inc_crosstab,
  aes(
    x = rent_burden_band,
    y = percent / 100
  )
) +
  # 1️⃣ Base layer: all non-CITY==0 points (solid black)
  geom_point(
    data = subset(df_low_inc_crosstab, CITY != 0),
    color = "black",
    size = 2,
    alpha = 0.9
  ) +
  # 2️⃣ Highlight layer: CITY==0 (larger, hollow)
  geom_point(
    data = subset(df_low_inc_crosstab, CITY == 0),
    shape = 21,              # hollow circle
    stroke = 1.3,            # line thickness of circle border
    size = 4,                # larger point
    color = "black",
    fill = "white"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Share of Multifamily Households by Rent Burden",
    x = "Rent Burden Band (% of Income)",
    y = "Percent of Multifamily Households"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


ggplot(
  df_low_inc_crosstab |> filter(CITY == 5350), 
  aes(x = rent_burden_band, y = percent/100)) +
  geom_point(size = 2, color = "#0072B2") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Share of Multifamily Households by Rent Burden (Low-Income Renters, 2023)",
    x = "Rent Burden Band (% of Income)",
    y = "Percent of Multifamily Households"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )


###
# Try 2023
df <- ipums_person |>
  filter(
    YEAR == 2023,
    OWNERSHP == 2, # renters only
    PERNUM == 1, # hoh
    !is.na(RENT),
    !is.na(hhinc_harmonized),
    hhinc_harmonized > 0
  ) |>
  mutate(
    rent_burden = (RENT * 12) / hhinc_harmonized,
    is_multifam = as.integer(is_multifam),
    n_child_group = if_else(n_child >= 8, 8L, n_child)
  ) |>
  filter(rent_burden > 0, rent_burden < 2) |>  # exclude impossible values
  collect() |>
  mutate(
    # Bin rent burden into 5% bands (0–4.9, 5–9.9, …)
    rent_burden_band = cut(
      rent_burden,
      breaks = seq(0, 2, by = 0.05),
      labels = sprintf("%g–%g",
                       seq(0, 1.95, by = 0.05) * 100,
                       seq(0.049, 2, by = 0.05) * 100),
      include.lowest = TRUE,
      right = FALSE
    )
  )

df_crosstab <- crosstab_percent(
  df,
  wt_col = "HHWT_hh",                       # or "HHWT" if you want household weighting
  group_by = c("rent_burden_band", "is_multifam"),
  percent_group_by = c("is_multifam")
) |>
  filter(is_multifam == 1, !is.na(rent_burden_band))  # drop the NA bin

ggplot(
  df_crosstab, 
  aes(x = rent_burden_band, y = percent/100)) +
  geom_point(size = 2, color = "#0072B2") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Share of renter households that are doubled up, \nby rent burden, 2023",
    x = "Rent Burden Band (% of Income)",
    y = "Percent of Renter Households"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

### multiple years
library(dplyr)
library(ggplot2)
library(scales)
library(glue)
library(patchwork)   # for combining plots side-by-side or stacked

years <- c(1960, 1970, 1980, 1990, 2000, 2010, 2020, 2023)

plots <- list()

# ---- Step 1: Loop through years ----
for (yr in years) {
  df <- ipums_person |>
    filter(
      YEAR == yr,
      OWNERSHP == 2,          # renters only
      PERNUM == 1,            # household head
      !is.na(RENT),
      !is.na(hhinc_harmonized),
      hhinc_harmonized > 0
    ) |>
    mutate(
      rent_burden = (RENT * 12) / hhinc_harmonized,
      is_multifam = as.integer(is_multifam)
    ) |>
    filter(rent_burden > 0, rent_burden < 2) |>
    collect() |>
    mutate(
      rent_burden_band = cut(
        rent_burden,
        breaks = seq(0, 2, by = 0.05),
        labels = sprintf("%g–%g",
                         seq(0, 1.95, by = 0.05) * 100,
                         seq(0.049, 2, by = 0.05) * 100),
        include.lowest = TRUE,
        right = FALSE
      )
    )
  
  df_crosstab <- crosstab_percent(
    df,
    wt_col = "HHWT_hh",
    group_by = c("rent_burden_band", "is_multifam"),
    percent_group_by = c("is_multifam")
  ) |>
    filter(is_multifam == 1, !is.na(rent_burden_band))
  
  # ---- Step 2: Store each plot ----
  p <- ggplot(
    df_crosstab,
    aes(x = rent_burden_band, y = percent / 100)
  ) +
    geom_point(size = 2, color = "#0072B2") +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 0.25)   # <-- sets y-axis limits (here 0–25%)
    ) +
    labs(
      title = glue("Doubled-Up Renters by Rent Burden, {yr}"),
      x = "Rent Burden Band (% of Income)",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  plots[[as.character(yr)]] <- p
}

# ---- Step 3: Combine all plots on common y-axis ----
combined_plot <- wrap_plots(plots, ncol = 2) +
  plot_annotation(title = "Share of Renter Households that are Doubled-Up by Rent Burden (1960–2023)",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

combined_plot
