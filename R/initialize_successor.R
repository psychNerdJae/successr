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
#' @return A tibble containing as many observations as there are cells in an
#'     NxN matrix, where N=number of nodes. The column `successor_value`
#'     encodes whatever initialized successor value is chosen.
#'
#' @import dplyr
#' @import tidyr
#' @import igraph
#' @import tidygraph
#' @export
#'
#' @examples
#' karate <- tidygraph::as_tbl_graph(successr::karate, directed = F)
#' initialize_successor(karate)
#'
initialize_successor <- function(input_tidygraph, initialize_with = 0) {

  output_df <- graph_to_adjlist(input_tidygraph) %>%
    mutate(successor_value = initialize_with)

  return (output_df)
}

