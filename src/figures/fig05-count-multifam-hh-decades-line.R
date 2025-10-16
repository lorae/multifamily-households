# fig05-count-multifam-hh-decades-line.R

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("scales")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_household <- tbl(con, "ipums_household")

# ----- Step 1: Arrange data ----- #
multifam_decade <- crosstab_count(
  data = ipums_household |> filter(GQ %in% c(0,1,2)) |> filter(is_multifam),
  wt_col = "HHWT",
  group_by = c("n_multifam", "YEAR"),
) |>
  arrange(YEAR) |>
  collect()

# ----- Step 2: Graph ----- #
fig05 <- multifam_decade |>
  ggplot(aes(
    x = YEAR,
    y = weighted_count,
    color = factor(n_multifam),
    group = n_multifam
  )) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(
    name = "Families per Household",
    values = c(
      "2" = "#FDE725",  # bright yellow
      "3" = "#F8961E",  # orange
      "4" = "#D1495B",  # red
      "5" = "#7209B7"   # purple
    )
  ) +
  scale_x_continuous(breaks = seq(1900, 2020, by = 10)) +
  scale_y_continuous(labels = scales::comma) + 
  labs(
    title = "Number of Multifamily Households by Year",
    x = "Year",
    y = "Number of Households"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

fig05

# ----- Step 3: Save figure and data ----- #
write.csv(
  multifam_decade,
  "output/figure-data/fig05-count-multifam-hh-decades-line.csv",
  row.names = FALSE
)

ggsave(
  "output/figures/fig05-count-multifam-hh-decades-line.jpeg", 
  fig05, 
  width = 6, 
  height = 6, 
  dpi = 300,
  scale = 1.25
  )
