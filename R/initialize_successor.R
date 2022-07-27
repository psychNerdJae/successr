#' @title Initialize successor values
#'
#' @description
#' The successor counts must be initialized to some value. All things equal,
#' the value `0` tends to be pretty good.
#'
#' @param input Formatted either as `tidygraph::tbl_graph`, OR a scalar with
#' the number of nodes. This function will not work with other kinds of network
#' representations, e.g. from `igraph`. Make sure that your input graph is
#' either directed or undirected, as you'd like it.
#' @param initialize_with Scalar, defaults to `0`.
#' @return A square NxN matrix, where N=number of nodes.
#'
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' tidygraph::tbl_graph(edges = successr::karate, directed = F) %>%
#'   successr::initialize_successor()
#'
initialize_successor <- function(input, initialize_with = 0) {

  if (is.numeric(input)) {
    if (length(input) != 1) {
      stop("If providing a scalar, specify the total number of nodes.")
    }
    square_size <- input
  } else if (any(class(input) == "tbl_graph")) {
    square_size <- dim(convert_graph_to_matrix(input))[1]
  } else {
    stop("Input must be a scalar or `tidygraph::tbl_graph`.")
  }

  return (matrix(initialize_with, square_size, square_size))
}

