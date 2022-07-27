#' @title Translate between successor formats
#'
#' @description
#' Technically speaking, the values encoded in a successor matrix are (this is a
#' mouthful) discounted expected counts of future state occupancies, given a
#' starting state. Whew. More simply: they're counts of how often you'd expect
#' to end up in a particular state, given that you started out in a particular
#' state. These counts depend on the number of "steps" the agent is looking into
#' the future, which is determined by the successor horizon term \eqn{\gamma}.
#'
#' There's a deterministic relationship between \eqn{\gamma} and the number of
#' steps the agent looks into the future (lookahead). This means that
#' \eqn{\gamma} can be used to normalize the successor matrix so that it encodes
#' probabilities instead of counts. The equation to translate between the two is
#' \eqn{lookahead = 1 / (1 - \gamma)}. In the case of \eqn{\gamma = 0.8}, for
#' example, the successor algorithm looks five steps into the future.
#'
#' We can convert counts into probabilities by scalar-multiplying the matrix
#' \eqn{M} by the normalization factor \eqn{1 - \gamma}. We can inversely
#' convert probabilities into counts: \eqn{M * (1 / 1 - \gamma)}.
#'
#' Yes, it's that easy, but beware that your probabilities might not sum to one
#' (!!), due to computational limits on numeric precision or a non-converged
#' (non-asymptotic) matrix.
#'
#' @examples
#' TODO
#'
#' @param input The learned successor values, either in a dataframe or matrix.
#' @param relation_value_col If your input is a dataframe, specify the name
#' of the column where the relational strengths are encoded.
#' @param successor_horizon_col If your input is a dataframe, specify the name
#' of the column specifying the successor horizon gamma.
#' @param successor_horizon_scalar If your input is a matrix, specify the gamma.

#' @export
#' @rdname translate_counts_to_probabilities
translate_gamma_to_lookahead <- function(successor_horizon) {
  return (1 / (1 - successor_horizon))
}

#' @export
#' @rdname translate_counts_to_probabilities
translate_lookahead_to_gamma <- function(n_steps) {
  return (1 - (1 / n_steps))
}

#' @export
#' @rdname translate_counts_to_probabilities
translate_counts_to_probabilities <- function(
  input,
  relation_value_col = NULL,
  successor_horizon_col = NULL,
  successor_horizon_scalar = NULL
) {

  if (is.data.frame(input)) {
    output <- input %>%
      mutate(
        {{relation_value_col}} :=
          {{relation_value_col}} * (1 - {{successor_horizon_col}})
      )
  } else if (is.matrix(input)) {
    output <- input * (1 - successor_horizon_scalar)
  } else {
    stop("Input data is neither dataframe nor matrix.")
  }

  return (output)
}

#' @export
#' @rdname translate_counts_to_probabilities
translate_probabilities_to_counts <- function(
  input,
  relation_value_col = NULL,
  successor_horizon_col = NULL,
  successor_horizon_scalar = NULL
) {

  if (is.data.frame(input)) {
    output <- input %>%
      mutate(
        {{relation_value_col}} :=
          {{relation_value_col}} * (1 / (1 - {{successor_horizon_col}}))
      )
  } else if (is.matrix(input)) {
    output <- input * (1 / (1 - successor_horizon_scalar))
  } else {
    stop("Input data is neither dataframe nor matrix.")
  }

  return (output)
}

