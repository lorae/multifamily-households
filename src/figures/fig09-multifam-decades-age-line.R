# fig09-multifam-decades-age-line
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

multifam_decade_age <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("is_multifam", "YEAR", "age_bucket"),
  percent_group_by = c("YEAR", "age_bucket")
) |>
  filter(is_multifam) |>
  arrange(YEAR)

# ----- Graph ----- #
fig09 <- multifam_decade_age |>
  ggplot(aes(
    x = YEAR,
    y = percent / 100,
    color = age_bucket,
    group = age_bucket
  )) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    name = "Age Group",
    values = c(
      "17 or younger" = "#5BC0EB",  # light sky blue
      "18-29"         = "#0081A7",  # medium teal
      "30-49"         = "#F4D35E",  # warm yellow
      "50-65"         = "#EE964B",  # amber
      "65 and older"  = "#9B2226"   # deep red-brown
    )
  ) +
  scale_x_continuous(breaks = seq(1900, 2020, by = 10)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Share of Americans Living in Multifamily Households by Age Group",
    x = "Year",
    y = "Percent of Americans"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

fig09

# ----- Step 3: Save data and plot ----- #

write_csv(
  multifam_decade_age,
  "output/figure-data/fig09-multifam-decades-age-line.csv"
)

ggsave(
  filename = "output/figures/fig09-multifam-decades-age-line.jpeg",
  plot = fig09,
  width = 6,
  height = 4,
  dpi = 500,
  scale = 1.5
)
