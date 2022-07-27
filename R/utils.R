#' @title Utilities
#'
#' @description
#' Utility functions for developer convenience, not intended for users.
#' Also allows for NAMESPACE imports to be collected in one place.
#' Should not be exported.
#' @importFrom magrittr `%>%`

user_col_to_string <- function(user_col_in_double_brackets) {
  # If this function (f1) is used inside another function (f2), and is meant to
  # return a string for the f2 column name provided by the user, remember to
  # wrap the f1 argument in double brackets
  return(rlang::as_string(rlang::ensym(user_col_in_double_brackets)))
}
