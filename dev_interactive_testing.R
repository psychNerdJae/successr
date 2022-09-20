# Development
library(devtools)
library(testthat)
# library(rlang)

# Load package as-is
load_all()

# Re-compile documentation
# roxygen2::roxygenize()

# Reading in the karate data
# load(here::here("data", "karate.rda"))

karate_graph <- karate %>%
  tidygraph::tbl_graph(
    edges = .,
    directed = FALSE
  )

#
karate_graph %>%
  convert_graph_to_adjlist(edge)

obs <- karate_graph %>%
  simulate_pairwise(n_reps = 10)

karate_graph %>%
  initialize_successor() %>%
  learn_successor(
    observations = obs,
    relation_value_col = sr,
    learning_rates = c(0.1, 0.2),
    successor_horizons = 0.8,
    bidirectional = TRUE
  )
