#' @title Simulate observations from a network
#'
#' @description
#' `simulate_pairwise`: Generate N pairwise observations of each relationship,
#' like in a (balanced) laboratory experiment.
#' `simulate_random_walk`: Generate a random walk of N length.
#'
#' @param input For `simulate_pairwise`, this can be formatted either as a
#' `tidygraph::tbl_graph`, OR an adjlist. For `simulate_random_walk`, you can
#' only use `tidygraph::tbl_graph`. This function will not work with other kinds
#' of network representations, e.g. from `igraph`. Make sure that your input
#' graph is either directed or undirected, as you'd like it.
#' @param n_reps Number of repetitions.
#' @param relation_value_col The name of the column that encodes (or will
#' encode) the strength of relationship between two nodes. For example, in an
#' adjacency representation, this might be zeroes (not friends) and ones
#' (yes friends).
#' @param start_here In the random walk, the starting node's numeric ID.
#'
#' @examples
#' karate_graph <- tidygraph::tbl_graph(edges = successr::karate, directed = F)
#' karate_obs <- karate_graph %>% successr::simulate_experiment(1000)

#' @export
simulate_pairwise <- function(input, n_reps, relation_value_col=NULL) {

  # Get the edgelist
  if (any(class(input) == "tbl_graph")) {
    edgelist <- input %>%
      tidygraph::activate("edges") %>%
      tidygraph::as_tibble() %>%
      dplyr::select(from, to)
  } else if (!any(class(input) %in% class(tibble()))) {
    stop("Input is not `data.frame`-like")
  } else {
    these_cols <- colnames(input)
    if (!any(these_cols == "from") | !any(these_cols == "to")) {
      stop("Your dataframe does not contain the columns `from` and `to`.")
    }

    if (!is.numeric(input$from) | !is.numeric(input$to)) {
      stop("One or both of the columns `from` and `to` contain non-numbers.")
    }

    if (!any(these_cols == user_col_to_string({{relation_value_col}}))) {
      stop("Your dataframe does not contain that relation value column.")
    }

    if (
      input %>%
      dplyr::select({{relation_value_col}}) %>%
      tibble::deframe() %>%
      class()
      != "numeric"
    ) {
      stop("Your relation value column contains non-numbers.")
    }

    n_nodes <- with(input, max(from, to))
    if (nrow(input) != n_nodes*(n_nodes-1) + n_nodes) {
      # adjlist MUST include identity (from == to)
      stop("You have the wrong number of observations in your adjlist.")
    }

    edgelist <- input %>%
      dplyr::filter({{relation_value_col}} != 0) %>%
      dplyr::filter(from < to) %>%
      dplyr::select(from, to)
  }

  # Repeat N times
  return (
    tidyr::expand_grid(rep = 1:n_reps, edgelist) %>%
      dplyr::group_by(rep) %>%
      dplyr::slice_sample(prop = 1) %>%
      dplyr::ungroup()
  )
}

#' @export
#' @rdname simulate_pairwise
simulate_random_walk <- function(input, n_obs, start_here = NULL) {

  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop(
      "Package \"tidygraph\" must be installed to use this function.",
      call. = FALSE
    )
  }

  # Pick starting place at random
  n_nodes <- igraph::vcount(input)
  if (is.null(start_here)) {
    start_here <- sample(1:n_nodes, 1)
  }

  # Get full sequence
  this_walk <- igraph::random_walk(
    graph = input,
    start = start_here,
    # 10 steps produces 9 transitions
    # so to get N observations, we need N+1 steps
    steps = n_obs + 1
  ) %>%
    igraph::as_ids() %>%
    as.numeric()

  # Parse into pairs
  return (
    this_walk %>%
      tibble::enframe(name = NULL, value = "from") %>%
      dplyr::mutate(to = dplyr::lead(from)) %>%
      tidyr::drop_na()
  )
}
