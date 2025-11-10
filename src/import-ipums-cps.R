# import-ipums-cps.R
#
# This script processes raw IPUMS data and saves it in a DuckDB file.
#
# Input:
# -  makes API call to IPUMS USA. Be sure to follow Part B of project set-up
#    in README.md before running - this script reads an environment variable from 
#    .Renviron
#

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("glue")

if (!file.exists(".Renviron")) {
  stop(".Renviron file needed for this code to run. Please refer to Part B of the README file for configuration instructions.")
} 

# Read API key from project-local .Renviron
readRenviron(".Renviron") # Force a re-read each run
api_key <- Sys.getenv("IPUMS_API_KEY")

if (api_key == "" || api_key == "your_ipums_api_key") {
  stop(".Renviron file exists, but IPUMS API key has not been added. Please refer to Part B of the README file for configuration instructions.")
}

print(paste0("IPUMS API key: ", api_key))
set_ipums_api_key(api_key)

# Set the destination directories for the IPUMS data pull
download_dir <- "data/raw-microdata"
db_dir <- "data/db"

# ----- Step 1: Define, submit, and wait for data extract ----- #
# Browse available samples and their aliases
get_sample_info("cps") |> print(n=800) 


# Define extract
ipums_extract <- define_extract_micro(
  description = "Multifamily households, 1960 - 2023",
  collection = "cps",
  samples = get_sample_info("cps")|> arrange(name) |> tail(100) |> pull(name),
  variables = c(
    # Household-level
    "NUMPREC", "NFAMS", "NCOUPLES", "NMOTHERS", "NFATHERS", "MULTGEN", "ROOMS", "BEDROOMS", 
    # Person-level
    "PERNUM", "PERWT", "LINENO", "RELATE",
    "SUBFAM", "FAMSIZE", "FAMUNIT",
    "MOMLOC", "POPLOC", "PELNDAD", "PELNMOM", "PEMOMTYP", 
    "SPLOC",  "ASPOUSE", "PECOHAB",
    "NCHILD",  "NSIBS",
    "SEX", "AGE"
  )
)