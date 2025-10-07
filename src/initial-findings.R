# Produces quick facts used in the paper
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums")

hhsize_decade <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("YEAR")
) 

ggplot(hhsize_decade, aes(x = YEAR, y = weighted_mean)) +
  geom_line(size = 1.2, color = "#0072B2") +
  geom_point(size = 2, color = "#0072B2") +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  labs(
    title = "Persons per Household in the United States, 1900–2023",
    x = NULL,
    y = "Persons per Household"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )
