# process-person-level-ipums.R
#
# This script adds bucket columns to raw (person-level) data.
# It reads data from the "ipums" table in `/db/ipums-raw.duckdb` and writes processed
# data to the "ipums-bucketed" table in `/db/ipums-processed.duckdb`.
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("dbplyr")

devtools::load_all("../demographr")

# ----- Step 1: Connect to the database ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_db <- tbl(con, "ipums")

# For data validation: count number of rows, to ensure none are dropped later
obs_count <- ipums_db |>
  summarise(count = n()) |>
  pull()


# ----- Step 2: Add columns ----- #

ipums_person <- ipums_db |>
  mutate(
    # Top-code at 5, since 1940- 1970 has most restrictive top-code
    n_multifam = case_when(
      NFAMS == 0 ~ 0,
      NFAMS == 1 ~ 1,
      NFAMS == 2 ~ 2,
      NFAMS == 3 ~ 3,
      NFAMS == 4 ~ 4,
      NFAMS >= 5 ~ 5
    ),
    is_multifam = case_when(
      n_multifam == 0 ~ NA,
      n_multifam == 1 ~ FALSE,
      n_multifam >= 2 ~ TRUE
    ),
    # Top-code at 9, which is the top-code for 2006 and earlier (most restrictive)
    room = case_when(
      ROOMS < 9 ~ ROOMS,
      ROOMS >= 9 ~ 9
    ),
    # Recode integers and top-code at 56, which is the top-code for 2000 and earlier (most restrictive)
    bedroom = case_when(
      BEDROOMS == 0 ~ NA_integer_,
      BEDROOMS == 1 ~ 1, # efficiencies / studios: we classify as 1 bedroom
      BEDROOMS == 2 ~ 1,
      BEDROOMS == 3 ~ 2,
      BEDROOMS == 4 ~ 3,
      BEDROOMS == 5 ~ 4,
      BEDROOMS >= 6 ~ 5
    ),
    # Persons per bedroom
    ppbr = NUMPREC / bedroom,
    # Group people into age buckets
    # Note some years top code at age 90
    age_bucket = case_when(
      AGE < 18 ~ "17 or younger",
      AGE >= 18 & AGE < 30 ~ "18-29",
      AGE >= 30 & AGE < 50 ~ "30-49",
      AGE >= 50 & AGE < 65 ~ "50-65",
      AGE >= 65 ~ "65 and older"
    ),
    is_hispan = case_when(
      HISPAN == 9 ~ NA,
      HISPAN == 0 ~ FALSE,
      HISPAN == 1 ~ TRUE,
      HISPAN == 2 ~ TRUE,
      HISPAN == 3 ~ TRUE,
      HISPAN == 4 ~ TRUE
    ),
    race_bucket = case_when(
      RACE == 1 ~ "white",
      RACE == 2 ~ "black",
      RACE == 3 ~ "aian",
      RACE %in% c(4, 5, 6) ~ "aapi",
      RACE %in% c(8, 9) ~ "multi",
      RACE == 7 ~ "other"
    ),
    race_eth = case_when(
      is_hispan ~ "Hispanic", # All Hispanics labelled as "Hispanic" regardless of race
      race_bucket == "black" ~ "Black",
      race_bucket == "aapi" ~ "AAPI",
      race_bucket == "aian" ~ "AIAN",
      race_bucket == "multi" ~ "Multiracial",
      race_bucket == "white" ~ "White",
      race_bucket == "other" ~ "Other"
    ),
    # See README for documentation on this procedure.
    # Source: https://usa.ipums.org/usa/cpi99.shtml
    inctot_harmonized = case_when(
      # Missing values
      INCTOT == 9999999 ~ NA_real_,
      
      # Bottom-coded values — harmonized to 2023 bottom code
      YEAR == 1960 & INCTOT <= -1911 ~ -19998,
      YEAR == 1970 & INCTOT <= -2409 ~ -19998,
      YEAR == 1980 & INCTOT <= -4766 ~ -19998,
      YEAR == 1990 & INCTOT <= -8139 ~ -19998,
      YEAR == 2000 & INCTOT <= -11312 ~ -19998,
      YEAR == 2001 & INCTOT <= -11625 ~ -19998,
      YEAR == 2002 & INCTOT <= -11813 ~ -19998,
      YEAR == 2003 & INCTOT <= -12087 ~ -19998,
      YEAR == 2004 & INCTOT <= -12402 ~ -19998,
      YEAR == 2005 & INCTOT <= -12824 ~ -19998,
      YEAR == 2006 & INCTOT <= -13243 ~ -19998,
      YEAR == 2007 & INCTOT <= -13606 ~ -19998,
      YEAR == 2008 & INCTOT <= -14133 ~ -19998,
      YEAR == 2009 & INCTOT <= -14078 ~ -19998,
      YEAR == 2010 & INCTOT <= -14318 ~ -19998,
      YEAR == 2011 & INCTOT <= -14762 ~ -19998,
      YEAR == 2012 & INCTOT <= -15067 ~ -19998,
      YEAR == 2013 & INCTOT <= -15299 ~ -19998,
      YEAR == 2014 & INCTOT <= -15538 ~ -19998,
      YEAR == 2015 & INCTOT <= -15560 ~ -19998,
      YEAR == 2016 & INCTOT <= -15762 ~ -19998,
      YEAR == 2017 & INCTOT <= -16110 ~ -19998,
      YEAR == 2018 & INCTOT <= -16499 ~ -19998,
      YEAR == 2019 & INCTOT <= -16777 ~ -19998,
      YEAR == 2020 & INCTOT <= -16986 ~ -19998,
      YEAR == 2021 & INCTOT <= -17787 ~ -19998,
      YEAR == 2022 & INCTOT <= -19225 ~ -19998,
      YEAR == 2023 & INCTOT <= -19998 ~ -19998,
      
      # Top-coded values — harmonized to 1960 top code
      YEAR == 1960 & INCTOT >= 23886 ~ 250000,
      YEAR == 1970 & INCTOT >= 30121 ~ 250000,
      YEAR == 1980 & INCTOT >= 59586 ~ 250000,
      YEAR == 1990 & INCTOT >= 101749 ~ 250000,
      YEAR == 2000 & INCTOT >= 141417 ~ 250000,
      YEAR == 2001 & INCTOT >= 145324 ~ 250000,
      YEAR == 2002 & INCTOT >= 147678 ~ 250000,
      YEAR == 2003 & INCTOT >= 151105 ~ 250000,
      YEAR == 2004 & INCTOT >= 155045 ~ 250000,
      YEAR == 2005 & INCTOT >= 160317 ~ 250000,
      YEAR == 2006 & INCTOT >= 165557 ~ 250000,
      YEAR == 2007 & INCTOT >= 170087 ~ 250000,
      YEAR == 2008 & INCTOT >= 176680 ~ 250000,
      YEAR == 2009 & INCTOT >= 175997 ~ 250000,
      YEAR == 2010 & INCTOT >= 178992 ~ 250000,
      YEAR == 2011 & INCTOT >= 184548 ~ 250000,
      YEAR == 2012 & INCTOT >= 188361 ~ 250000,
      YEAR == 2013 & INCTOT >= 191259 ~ 250000,
      YEAR == 2014 & INCTOT >= 194247 ~ 250000,
      YEAR == 2015 & INCTOT >= 194523 ~ 250000,
      YEAR == 2016 & INCTOT >= 197046 ~ 250000,
      YEAR == 2017 & INCTOT >= 201399 ~ 250000,
      YEAR == 2018 & INCTOT >= 206259 ~ 250000,
      YEAR == 2019 & INCTOT >= 209739 ~ 250000,
      YEAR == 2020 & INCTOT >= 212345 ~ 250000,
      YEAR == 2021 & INCTOT >= 222358 ~ 250000,
      YEAR == 2022 & INCTOT >= 240334 ~ 250000,
      YEAR == 2023 & INCTOT >= 250000 ~ 250000,
      
      # All other values — adjusted by inflation factor (converted to 2023 $)
      YEAR == 1960 ~ INCTOT * 10.466,
      YEAR == 1970 ~ INCTOT * 8.300,
      YEAR == 1980 ~ INCTOT * 4.196,
      YEAR == 1990 ~ INCTOT * 2.457,
      YEAR == 2000 ~ INCTOT * 1.768,
      YEAR == 2001 ~ INCTOT * 1.720,
      YEAR == 2002 ~ INCTOT * 1.693,
      YEAR == 2003 ~ INCTOT * 1.654,
      YEAR == 2004 ~ INCTOT * 1.612,
      YEAR == 2005 ~ INCTOT * 1.559,
      YEAR == 2006 ~ INCTOT * 1.510,
      YEAR == 2007 ~ INCTOT * 1.470,
      YEAR == 2008 ~ INCTOT * 1.415,
      YEAR == 2009 ~ INCTOT * 1.420,
      YEAR == 2010 ~ INCTOT * 1.397,
      YEAR == 2011 ~ INCTOT * 1.355,
      YEAR == 2012 ~ INCTOT * 1.327,
      YEAR == 2013 ~ INCTOT * 1.307,
      YEAR == 2014 ~ INCTOT * 1.287,
      YEAR == 2015 ~ INCTOT * 1.285,
      YEAR == 2016 ~ INCTOT * 1.269,
      YEAR == 2017 ~ INCTOT * 1.241,
      YEAR == 2018 ~ INCTOT * 1.212,
      YEAR == 2019 ~ INCTOT * 1.192,
      YEAR == 2020 ~ INCTOT * 1.177,
      YEAR == 2021 ~ INCTOT * 1.124,
      YEAR == 2022 ~ INCTOT * 1.040,
      YEAR == 2023 ~ INCTOT * 1.000,
      
      TRUE ~ NA_real_
    )
  )

# ----- Step 3: Compute, save, close out the connection ----- #

# Create a new table to write processed columns to
compute(
  ipums_person,
  name = "ipums_person",
  temporary = FALSE,
  overwrite = TRUE
)

# Validate no rows were dropped
validate_row_counts(
  db = tbl(con, "ipums_person"),
  expected_count = obs_count,
  step_description = "ipums_person db was created"
)

dbDisconnect(con)