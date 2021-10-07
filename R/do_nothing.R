#' @title Does nothing
#' @name do_nothing
#'
#' @description
#' Does nothing. Exists for developer convenience, but pointless for users.
#' Allows for NAMESPACE imports to be collected in one place.
#' Should not be exported.
#'
#' @param input Obligatory input.
#' @return Returns the input.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom tibble enframe
#' @importFrom stringr str_remove
#' @importFrom igraph vcount random_walk as_ids
#' @importFrom tidygraph activate as_tibble with_graph graph_is_directed
#'
do_nothing <- function(input) {
  return (input)
}

