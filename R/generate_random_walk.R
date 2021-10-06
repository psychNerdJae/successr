#' @title Random walk around a graph
#' @name generate_random_walk
#'
#' @description
#' Generate a random walk around a graph. Does *not* factor edge weights into
#' the walk (i.e., will transition to any successor node with equal
#' probability).
#'
#' @param input_tidygraph A `tbl_graph` representation from `tidygraph`.
#'     This function may not work with other kinds of network representations,
#'     e.g. from `igraph`. Make sure that your input graph is either directed
#'     or undirected, as you'd like it.
#' @param n_obs Number of observations (i.e., steps in the random walk).
#' @param start_here Numeric node ID. By default, picks a random starting point.
#' @return A tibble containing all observations.
#'
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' successr::karate %>%
#'     tidygraph::as_tbl_graph(directed = F) %>%
#'     generate_random_walk(100)
#'
generate_random_walk <- function(input_tidygraph, n_obs, start_here = NULL) {

  # Pick starting place at random
  if (is.null(start_here)) {
    start_here <- sample(1:vcount(input_tidygraph), 1)
  }

  # Get full sequence
  this_walk <- random_walk(
    graph = input_tidygraph,
    start = start_here,
    # 10 steps produces 9 transitions
    # so to get N observations, we need N+1 steps
    steps = n_obs + 1
  ) %>%
    as_ids() %>%
    as.numeric()

  # Parse into pairs
  output <- this_walk %>%
    enframe(name = NULL,
            value = "from") %>%
    mutate(to = lead(from)) %>%
    drop_na()

  return (output)
}

