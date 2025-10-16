# fig08-gq-decades-race-line
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
ipums_person <- tbl(con, "ipums_person") |>
  mutate(
    in_gq = GQ >= 3
  )

gq_decade_race <- crosstab_percent(
  data = ipums_person,
  wt_col = "PERWT",
  group_by = c("in_gq", "YEAR", "race_eth"),
  percent_group_by = c("YEAR", "race_eth")
) |>
  filter(in_gq) |>
  filter(race_eth %in% c("White", "Black", "AAPI", "Hispanic", "AIAN")) |>
  arrange(YEAR)

# ----- Graph ----- #
fig08 <- gq_decade_race |>
  ggplot(aes(
    x = YEAR,
    y = percent/100,
    color = race_eth,
    group = race_eth
  )) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    name = "Race / Ethnicity",
    values = c(
      "White"    = "#7fc97f",
      "Black"    = "#beaed4",
      "Hispanic" = "#fdc086",
      "AAPI"     = "#ff9299",
      "AIAN"     = "#5c4522"
    )
  ) +
  scale_x_continuous(breaks = seq(1900, 2020, by = 10)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Share of Americans Living in Group Quarters by Race/Ethnicity",
    x = "Year",
    y = "Percent living in group quarters"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

fig08

# ----- Step 3: Save data and plot ----- #

write_csv(
  gq_decade_race,
  "output/figure-data/fig08-gq-decades-race-line.csv"
)

ggsave(
  filename = "output/figures/fig08-gq-decades-race-line.jpeg",
  plot = fig08,
  width = 6,
  height = 4,
  dpi = 500,
  scale = 1.5
)
