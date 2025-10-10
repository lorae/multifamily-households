# Clustering analysis
# Goal: Cluster household data to produce archetypal households
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")
library("purrr")
library("tidyr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_household <- tbl(con, "ipums_household")

set.seed(123)

# Prepare data for k-means: select variables and scale
# Variables to process
vars <- c(
  "n_spouse",
  "n_child",
  "n_parent",
  "n_grandchild",
  "n_other_rel",
  "n_non_rel"
)


ipums_household_tb <- ipums_household |>
  filter(GQ %in% c(0,1,2)) |>
  # TODO: Add household weights back (HHWT)
  select(all_of(vars)) |>
  collect() |>
  scale()

# ----- elbow
# Prepare a list to store all models
kmeans_results <- list()

# Loop over k values
for (k in 1:15) {
  message("Running k-means for k = ", k, " ...")
  km <- kmeans(ipums_household_tb, centers = k, nstart = 10)
  kmeans_results[[as.character(k)]] <- km
  message("Completed k = ", k)
}

# Extract within-cluster SS (for elbow plot)
wss <- sapply(kmeans_results, function(x) x$tot.withinss)

# Plot the elbow
plot(1:15, wss, type = "b",
     xlab = "Number of clusters (k)",
     ylab = "Total within-cluster SS",
     main = "Elbow Method")

k_fit <- kmeans_results[["6"]]  # retrieve model for k = 6

# 1. Cluster centers (in scaled units)
round(k_fit$centers, 2)

# 2. Cluster sizes
k_fit$size

# 3. Total within-cluster sum of squares
k_fit$tot.withinss

# 4. Between-cluster sum of squares ratio (a quick fit metric)
k_fit$betweenss / k_fit$totss


centers_unscaled <- sweep(k_fit$centers, 2, attr(ipums_household_tb, "scaled:scale"), "*")
centers_unscaled <- sweep(centers_unscaled, 2, attr(ipums_household_tb, "scaled:center"), "+")
round(centers_unscaled, 2)

