#' @title Initialize successor values
#'
#' @description
#' The successor counts must be initialized to some value. All things equal,
#' an identity matrix is a reasonable choice. Depending on the successor
#' horizon at which the agent learns, the expected count should theoretically
#' be scaled. However, a common use case of this library is to estimate
#' multiple successor horizons, in which case this is unfeasible. In these
#' situations, it probably makes a negligible difference (given sufficient
#' learning observations) to simply use an unscaled identity matrix.
#'
#' @param input Formatted either as `tidygraph::tbl_graph`, OR a scalar with
#' the number of nodes. This function will not work with other kinds of network
#' representations, e.g. from `igraph`. Make sure that your input graph is
#' either directed or undirected, as you'd like it.
#' @param successor_horizon Scalar in range [0, 1), defaults to `0`.
#' @return A square NxN matrix, where N=number of nodes.
#'
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' tidygraph::tbl_graph(edges = successr::karate_named, directed = F) %>%
#'   successr::initialize_successor()
#'
initialize_successor <- function(input, successor_horizon = 0) {

  if (is.numeric(input)) {
    if (length(input) != 1) {
      stop("If providing a scalar, specify the total number of nodes.")
    }
    square_size <- input
  } else if (any(class(input) == "tbl_graph")) {
    if (!requireNamespace("tidygraph", quietly = TRUE)) {
      stop(
        "Package \"tidygraph\" must be installed to use this function.",
        call. = FALSE
      )
    }
    square_size <- dim(convert_graph_to_matrix(input))[1]
  } else {
    stop(
      "Input must be a scalar (number of nodes), OR `tidygraph::tbl_graph`."
    )
  }

  return (
    diag(
      x = translate_gamma_to_lookahead(successor_horizon),
      nrow = square_size,
      ncol = square_size,
      names = FALSE
    )
  )
}

