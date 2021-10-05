#' @title Convert matrix into adjacency list
#' @name matrix_to_adjlist
#'
#' @description
#' An NxN square matrix can be alternatively represented as an adjacency list
#' containing NxN observations. This function performs that conversion.
#'
#' @param input_matrix A square NxN matrix, where N=number of nodes.
#' @param edge_col_name The name of the column encoding the relation between
#'     two given nodes. This is probably `edge` for adjacency matrices, or
#'     `successor_value` for matrices encoding successor relations.
#' @return A tibble with NxN observations, such that every row is a pairwise
#'     combination of two nodes (`from` and `to`), and a column encoding the
#'     relationship between them.
#'
#' @import dplyr
#' @import tidyr
#' @import igraph
#' @import tidygraph
#' @importFrom stringr str_remove
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' successr::karate %>%
#'     tidygraph::as_tbl_graph(directed = F) %>%
#'     graph_to_adjlist() %>%
#'     adjlist_to_matrix() %>%
#'     matrix_to_adjlist()
#'
matrix_to_adjlist <- function(input_matrix, edge_col_name = "edge") {

  output <- input_matrix %>%
    as.data.frame() %>%
    mutate(from = row_number()) %>%
    pivot_longer(cols = -from,
                 names_to = "to",
                 values_to = edge_col_name) %>%
    mutate(to = stringr::str_remove(to, "V"),
           to = as.numeric(to))

  return (output)
}

