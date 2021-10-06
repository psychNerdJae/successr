#' @title Convert adjacency list into matrix
#' @name adjlist_to_matrix
#'
#' @description
#' An adjacency contains NxN observations that can be alternatively represented
#' as an NxN square matrix. This function performs that conversion.
#'
#' @param input_df A tibble with NxN observations, such that every row is a
#'     pairwise combination of two nodes (`from` and `to`), and a column called
#'     `edge` encodes whether there is a relationship between them.
#' @return A square NxN matrix, where N=number of nodes.
#'
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' successr::karate %>%
#'     tidygraph::as_tbl_graph(directed = F) %>%
#'     graph_to_adjlist() %>%
#'     adjlist_to_matrix()
#'
adjlist_to_matrix <- function(input_df) {

  output <- input_df %>%
    select(from, to, edge) %>%
    pivot_wider(names_from = to, values_from = edge) %>%
    select(-from) %>%
    as.matrix() %>%
    unname()

  return (output)
}

