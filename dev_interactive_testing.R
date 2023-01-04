# Development
library(devtools)
library(testthat)
# library(rlang)

# Load package as-is
load_all()

# Re-compile documentation
# roxygen2::roxygenize()

g_numeric <- karate_numeric %>%
  tidygraph::tbl_graph(
    edges = .,
    directed = FALSE
  )

g_named <- karate_named %>%
  tidygraph::tbl_graph(
    edges = .,
    directed = FALSE
  )

# Note that "named" graphs get converted into numeric IDs
g_named %>%
  convert_graph_to_adjlist(edge)

g_named %>%
  convert_graph_to_matrix()

#
obs <- g_named %>%
  simulate_pairwise(n_reps = 10)

g_named %>%
  initialize_successor() %>%
  learn_successor(
    observations = obs,
    relation_value_col = sr,
    learning_rates = c(0.1, 0.2),
    successor_horizons = c(0.5, 0.8),
    bidirectional = TRUE
  )
