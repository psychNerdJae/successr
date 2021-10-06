#' @title Update successor values
#' @name update_successor
#'
#' @description
#' After observing a relation between two nodes (i.e., transitioning between
#' two states), update the successor values. This is a fairly low-level
#' function, which should probably not be used directly in most cases. Instead,
#' if you have a dataframe of observations, you can run that through the
#' function `learn_from_observations`. If you want to see how successor values
#' vary for different parameter values (but using the same observations), you
#' can do that using the function `learn_from_param_sweep`.
#'
#' Upon seeing a transition from \eqn{i} to \eqn{j}, the update equation for the
#' successor matrix \eqn{M} is \eqn{M(i) <- M(i) + \alpha \delta}, where
#' \eqn{\delta = onehot(i, j) + \gamma M(j) - M(i)}.
#'
#' Technically, \eqn{M} should be indexed like a matrix. But for simplicity,
#' I write it like a single-input function that returns the associated row.
#' Therefore, the successor algorithm updates values in a row-wise manner.
#'
#' The one-hot term is a vector the length of \eqn{M(i)}, which is filled with
#' zeros except for a single one (\eqn{1}) at the location \eqn{j}. Hence, the
#' one-hot vector encodes that when the agent was in state \eqn{i}, the next
#' observed state was \eqn{j}.
#'
#' Where does the "successor" part of "successor representation/features" come
#' from? That's a reference to the middle part of the update equation. When you
#' encode the relationship between \eqn{i, j}, that's entirely accounted for by
#' the one-hot vector. But, you may want to also encode longer-range relations,
#' such that your representation of \eqn{i} not only includes the relationship
#' with \eqn{j}, but also the relationship between \eqn{j, k}. Therefore, you
#' will end up with larger successor values for direct connections, and smaller
#' values for indirect (e.g., long-range) connections.
#'
#' The learning rate \eqn{\alpha} tempers how strongly the one-hot updates
#' the learned successor values. The lookahead horizon \eqn{\gamma} dictates
#' how strongly the successor state's relations are incorporated into the
#' update.
#'
#' @param input_matrix A square NxN matrix created by `initialize_successor`.
#' @param alpha Scalar corresponding to the learning rate bound in [0, 1].
#' @param gamma Scalar corresponding to the lookaround horizon bound in [0, 1).
#' @param previous_state Scalar corresponding to the previously-seen state.
#' @param current_state Scalar corresponding to the currently-seen state.
#' @param bidirectional Logical. Defaults to `FALSE`, which means that only
#'     the `from-to` relationship gets updated. If set to `TRUE`, this function
#'     will also update the `to-from` relationship.
#' @return A matrix with updated successor values, given the observation.
#'
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' successr::karate %>%
#'     tidygraph::as_tbl_graph(directed = F) %>%
#'     initialize_successor() %>%
#'     update_successor(0.1, 0.4, 1, 2, TRUE) %>%
#'     matrix_to_adjlist()
#'
update_successor <- function(input_matrix,
                             alpha, gamma,
                             previous_state, current_state,
                             bidirectional = FALSE) {

  # Create one-hot (row) vector signaling current state
  forward_onehot <- input_matrix[previous_state, ] * 0
  forward_onehot[current_state] <- 1

  # Compute prediction error
  forward_delta <- (
    forward_onehot +
      (gamma * input_matrix[current_state, ]) -
      input_matrix[previous_state, ]
  )

  # Update the successor values
  output <- input_matrix
  output[previous_state, ] <- (
    input_matrix[previous_state, ] + (alpha * forward_delta)
  )

  # Repeat in the other direction if needed
  if (bidirectional) {
    backward_onehot <- input_matrix[current_state, ] * 0
    backward_onehot[previous_state] <- 1

    backward_delta <- (
      backward_onehot +
        (gamma * input_matrix[previous_state, ]) -
        input_matrix[current_state, ]
    )

    output[current_state, ] <- (
      input_matrix[current_state, ] + (alpha * backward_delta)
    )
  }

  return (output)
}

