# process-ipums-cps.R
#
# This script  reads data from the "ipums-cps" table in `/db/ipums-raw.duckdb` and writes processed
# data to the "ipums-cps-bucketed" table in `/db/ipums-processed.duckdb`.
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("dbplyr")

devtools::load_all("../demographr")

# ----- Step 1: Connect to the database ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums_cps.duckdb")
ipums_db <- tbl(con, "ipums-cps")

# For data validation: count number of rows, to ensure none are dropped later
obs_count <- ipums_db |>
  summarise(count = n()) |>
  pull()

ipums_household <- ipums_db |>
  mutate(hhid = paste(YEAR, MONTH, SERIAL, sep = "-"))

# View one household
# Pick one random CPSID and view that household
rand_id <- ipums_household |>
  distinct(CPSID) |>
  collect() |>
  sample_n(1) |>
  pull(CPSID)

# Filter and view that household
hh_sample <- ipums_household |>
  filter(CPSID == rand_id) |>
  collect()

View(hh_sample)