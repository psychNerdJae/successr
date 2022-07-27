#' @title Learn successor relationships through observation
#'
#' @description
#' After observing relations between nodes (i.e., transitioning between
#' states), update the successor values.
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
#' @examples
#' `%>%` <- magrittr::`%>%`
#' karate_graph <- tidygraph::tbl_graph(edges = successr::karate, directed = F)
#' karate_obs <- karate_graph %>% successr::simulate_experiment(1000)
#' karate_graph %>%
#'   successr::initialize_successor() %>%
#'   successr::learn_successor(karate_obs, sr, c(0.1, 0.2), c(0, 0.8), TRUE)
#'
#' @param input Square NxN matrix
#' @param observations Dataframe with columns `from` and `to`
#' @param relation_value_col The name of the column that encodes (or will
#' encode) the strength of relationship between two nodes.
#' @param learning_rates (List of) scalar(s) in [0, 1]
#' @param successor_horizons (List of) scalar(s) in [0, 1)
#' @param bidirectional Logical. Defaults to `FALSE`, which means that only
#'     the `from-to` relationship gets updated. If set to `TRUE`, this function
#'     will also update the `to-from` relationship.
#' @return A matrix with updated successor values, given the observation(s).

#' @rdname learn_successor
learn_from_single_obs <- function(
  input,
  learning_rate, successor_horizon,
  previous_state, current_state,
  bidirectional = FALSE
) {

  if (!any(class(input) == "matrix")) {
    stop("Input is not a matrix.")
  }

  if (length(learning_rate) != 1 | length(successor_horizon) != 1) {
    stop("More than one learning rate and/or successor horizon specified.")
  }

  if (learning_rate < 0 | learning_rate > 1) {
    stop("Learning rate must be in interval [0, 1].")
  }

  if (successor_horizon < 0 | successor_horizon >= 1) {
    stop("Successor horizon must be in interval [0, 1).")
  }

  if ((previous_state %% 1 != 0) | (current_state %% 1 != 0)) {
    stop("States must be integer-like.")
  }

  # Create one-hot (row) vector signaling current state
  forward_onehot <- input[previous_state, ] * 0
  forward_onehot[current_state] <- 1

  # Compute prediction error
  forward_delta <- (
    forward_onehot +
      (successor_horizon * input[current_state, ]) -
      input[previous_state, ]
  )

  # Update the successor values
  output <- input
  output[previous_state, ] <- (
    input[previous_state, ] + (learning_rate * forward_delta)
  )

  # Repeat in the other direction if needed
  if (bidirectional) {
    backward_onehot <- input[current_state, ] * 0
    backward_onehot[previous_state] <- 1

    backward_delta <- (
      backward_onehot +
        (successor_horizon * input[previous_state, ]) -
        input[current_state, ]
    )

    output[current_state, ] <- (
      input[current_state, ] + (learning_rate * backward_delta)
    )
  }

  return (output)
}

#' @rdname learn_successor
learn_from_multiple_obs <- function(
  input,
  observations, relation_value_col,
  learning_rate, successor_horizon,
  bidirectional = FALSE
) {

  if (!any(class(input) == "matrix")) {
    stop("`input` must be provided as a square matrix.")
  }
  matrix_size <- dim(input)
  if (matrix_size[1] != matrix_size[2]) {
    stop("`input` must be provided as a square matrix.")
  }

  if (!any(class(observations) %in% class(tibble()))) {
    stop("`observations` must be a dataframe with columns `from` and `to`.")
  }

  these_cols <- colnames(observations)
  if (!any(these_cols == "from") | !any(these_cols == "to")) {
    stop("`observations` does not contain the columns `from` and `to`.")
  }

  obs_matrix <- as.matrix(select(observations, from, to))

  for (j in 1:nrow(observations)) {

    previous_state <- obs_matrix[j, 1]
    current_state <- obs_matrix[j, 2]

    input <- learn_from_single_obs(
      input,
      learning_rate,
      successor_horizon,
      previous_state, current_state,
      bidirectional
    )
  }

  output <- input %>%
    convert_matrix_to_adjlist({{relation_value_col}}) %>%
    mutate(
      learning_rate = learning_rate,
      successor_horizon = successor_horizon
    )

  return (output)
}

#' @export
#' @rdname learn_successor
learn_successor <- function(
  input,
  observations, relation_value_col,
  learning_rates, successor_horizons,
  bidirectional = FALSE
) {

  if (!is.numeric(learning_rates) | !is.numeric(successor_horizons)) {
    stop("Learning rates and successor horizons must be numeric.")
  }

  for (learning_rate in learning_rates) {
    for (successor_horizon in successor_horizons) {
      # Update successor values for one combination of parameter values
      this_successor <- learn_from_multiple_obs(
        input,
        observations,
        {{relation_value_col}},
        learning_rate, successor_horizon,
        bidirectional
      ) %>%
        mutate(
          learning_rate = learning_rate,
          successor_horizon = successor_horizon
        )

      # Return one giant dataframe for all combinations of parameter values
      if (exists("output") && is.data.frame(get("output"))) {
        output <- add_row(output, this_successor)
      } else {
        output <- this_successor
      }
    }
  }

  return (output)
}
