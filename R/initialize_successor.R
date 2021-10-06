#' @title Initialize successor values
#' @name initialize_successor
#'
#' @description
#' The successor counts must be initialized to some value. All things equal,
#' the value `0` tends to be pretty good.
#'
#' @param input_tidygraph A `tbl_graph` representation from `tidygraph`.
#'     This function will not work with other kinds of network representations,
#'     e.g. from `igraph`. Make sure that your input graph is either directed
#'     or undirected, as you'd like it.
#' @param initialize_with A scalar. Defaults to `0`.
#' @return A square NxN matrix, where N=number of nodes.
#'
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' successr::karate %>%
#'     tidygraph::as_tbl_graph(directed = F) %>%
#'     initialize_successor()
#'
initialize_successor <- function(input_tidygraph, initialize_with = 0) {

  input_matrix <- input_tidygraph %>%
    graph_to_adjlist() %>%
    adjlist_to_matrix()

  square_size <- dim(input_matrix)[1]

  output <- matrix(initialize_with, square_size, square_size)

  return (output)
}

