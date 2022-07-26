#' @title Does nothing
#' @name do_nothing
#'
#' @description
#' Utility functions for developer convenience, not intended for users.
#' Also allows for NAMESPACE imports to be collected in one place.
#' Should not be exported.
#' @import dplyr
#' @import tidyr

# REMOVE THESE
# @importFrom igraph vcount random_walk as_ids
# @importFrom tidygraph activate as_tibble with_graph graph_is_directed

user_col_to_string <- function(user_col_in_double_brackets) {
  # If this function (f1) is used inside another function (f2), and is meant to
  # return a string for the f2 column name provided by the user, remember to
  # wrap the f1 argument in double brackets
  return(rlang::as_string(rlang::ensym(user_col_in_double_brackets)))
}
