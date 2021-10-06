#' @title Convert probabilities into successor counts
#' @name probability_to_counts
#'
#' @description
#' De-Normalizes the successor matrix. Technically, the successor values are
#' (this is a mouthful) discounted expected counts of future state occupancies,
#' given a starting state. More simply: they're counts of how often you'd
#' expect to end up in a particular state, given that you started out in a
#' particular state.
#'
#' Much of the time, we're not particularly interested in the exact counts,
#' but rather probabilities. Interestingly enough, there's a tight coupling
#' between \eqn{\gamma}, the number of lookahead steps, and the normalization
#' constant.
#'
#' The value of \eqn{\gamma} dictates how many "time steps" are considered
#' when encoding successor states into the current state. The exact equation is
#' \eqn{lookahead = 1 / (1 - \gamma)}. In the case of \eqn{\gamma = 0.8},
#' the successor algorithm looks five steps into the future.
#'
#' We can convert counts into probabilities by scalar-multiplying the matrix
#' \eqn{M} by the normalization factor \eqn{1 - \gamma}. We can inversely
#' convert probabilities into counts: \eqn{M * (1 / 1 - \gamma)}.
#' Yes, it's that easy, but beware that your probabilities might not sum to one
#' (!!), due to computational limits on numeric precision or a non-converged
#' (non-asymptotic) matrix.
#'
#' @param successor_values The learned successor values. If your input is a
#'     dataframe, there must be a column named "gamma".
#' @param value_col_name If your input is a dataframe, specify the column name.
#' @param gamma_value If your input is a matrix, specify the gamma.
#' @return Returns the input.
#'
#' @export
#'
probability_to_counts <- function(successor_values,
                                  value_col_name = NULL,
                                  gamma_value = NULL) {

  if (is.data.frame(successor_values)) {
    output <- successor_values %>%
      mutate({{value_col_name}} := {{value_col_name}} * (1 / (1 - gamma)))
  } else if (is.matrix(successor_values)) {
    output <- successor_values * (1 / (1 - gamma_value))
  } else {
    stop("Input data is neither dataframe nor matrix.")
  }

  return (output)
}

