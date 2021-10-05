#' @title Simulate a basic experiment
#' @name generate_experiment
#'
#' @description
#' Generate N pairwise observations of each relationship, like in a (balanced)
#' laboratory experiment.
#'
#' @param input_tidygraph A `tbl_graph` representation from `tidygraph`.
#'     This function may not work with other kinds of network representations,
#'     e.g. from `igraph`. Make sure that your input graph is either directed
#'     or undirected, as you'd like it.
#' @param n_reps Number of repetitions.
#'
#' @import dplyr
#' @import tidyr
#' @import igraph
#' @import tidygraph
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' successr::karate %>%
#'     tidygraph::as_tbl_graph(directed = F) %>%
#'     generate_experiment(2)
#'
generate_random_walk <- function(input_tidygraph, n_reps) {

  # Get the edgelist
  edgelist <- input_tidygraph %>%
    activate("edges") %>%
    as_tibble() %>%
    select(from, to)

  # Repeat N times
  output <- expand_grid(rep = 1:n_reps,
                        edgelist) %>%
    group_by(rep) %>%
    slice_sample(prop = 1) %>%
    ungroup()

  return (output)
}

