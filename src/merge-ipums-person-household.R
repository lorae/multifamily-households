# merge-ipums-person-household.R
#
# Merge household-level attributes into person-level data.
# Each person gains columns from the ipums_household table (joined by YEAR + SERIAL).
# Writes the merged data to "ipums_person_with_hh" in the same DuckDB database.
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ipumsr")
devtools::load_all("../demographr")

# ----- Step 1: Connect to the database ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")

ipums_person <- tbl(con, "ipums_person")
ipums_household <- tbl(con, "ipums_household")

# For validation
person_count <- ipums_person |> summarise(count = n()) |> pull()

# ----- Step 2: Merge ----- #
ipums_merged <- ipums_person |>
  left_join(ipums_household, by = c("YEAR", "SERIAL"), suffix = c("", "_hh"))

# Optionally reorder or drop duplicates of household vars
# e.g., if some columns exist in both and you only want the household version:
# ipums_merged <- ipums_merged |> select(-NUMPREC, -room, -bedroom)

# ----- Step 3: Write merged table ----- #
compute(
  ipums_merged,
  name = "ipums_person_with_hh",
  temporary = FALSE,
  overwrite = TRUE
)

# Validate
validate_row_counts(
  db = tbl(con, "ipums_person_with_hh"),
  expected_count = person_count,
  step_description = "Merged person + household data table created"
)

dbDisconnect(con)
