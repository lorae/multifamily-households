# classifying-subfamilies
# This Script does some of the pre-processing for the R markdown file so that it
# doesn't have to work for as long.

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("dbplyr")
library("lubridate")

devtools::load_all("../demographr")

# ----- Step 1: Connect to the database ----- #

con <- dbConnect(duckdb::duckdb(), "data/db/ipums_cps.duckdb")
ipums_db <- tbl(con, "ipums-cps")
ipums_household <- ipums_db |>
  mutate(hhid = paste(YEAR, MONTH, SERIAL, sep = "-")) |>
  collect() |>
  mutate(
    date = make_date(YEAR, MONTH, 1),
    date_formatted = date |> format("%B %Y"),
    relate_encoded = case_when(
      RELATE == 101 ~ "HOH",
      RELATE %in% c(201, 202, 203) ~ "Spouse",
      RELATE == 301 ~ "Child",
      RELATE == 303 ~ "Stepchild",
      RELATE == 501 ~ "Parent",
      RELATE == 701 ~ "Sibling",
      RELATE == 901 ~ "Grandchild",
      RELATE == 1001 ~ "Other relative",
      RELATE == 1113 ~ "Partner / Roommate",
      RELATE %in% c(1114, 1116, 1117) ~ "Unmarried partner",
      RELATE == 1115 ~ "Housemate / roommate",
      RELATE == 1241 ~ "Roomer / boarder / lodger",
      RELATE == 1242 ~ "Foster child",
      RELATE == 1260 ~ "Other nonrelative"
    ),
    sex_encoded = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female"
    )
  ) |>
  select(CPSID, 
         date, 
         NUMPREC, PERNUM,  LINENO, relate_encoded, 
         AGE, sex_encoded, 
         MOMLOC, PELNMOM, 
         POPLOC, PELNDAD, 
         SPLOC, ASPOUSE, PECOHAB, 
         NSIBS) |>
  group_by(CPSID) |>
  mutate(
    MOMLOC_PERNUM = PERNUM[match(MOMLOC, LINENO)],
    POPLOC_PERNUM = PERNUM[match(POPLOC, LINENO)],
    SPLOC_PERNUM = PERNUM[match(SPLOC, LINENO)]
  ) |>
  ungroup()

saveRDS(ipums_household, "throughput/classifying-subfamilies.rds")

dbDisconnect(con)