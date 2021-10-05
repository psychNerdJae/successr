#' @title Update successor values
#' @name update_successor
#'
#' @description
#' After observing a relation between two nodes (i.e., transitioning between
#' two states), update the successor values.
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
#' @import dplyr
#' @import tidyr
#' @import igraph
#' @import tidygraph
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

