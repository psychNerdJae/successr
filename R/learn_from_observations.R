#' @title Learn successor values from observations
#' @name learn_from_observations
#'
#' @description
#' After observing relations between nodes (i.e., transitioning between
#' states), update the successor values. This function learns from many
#' observations. To update the successor values from a single observation,
#' use `update_successor` instead.
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
#' @param successor_matrix A square matrix created by `initialize_successor`.
#' @param observations A tibble with observations `from` a node `to` another.
#' @param input_alpha Scalar. Learning rate bound in [0, 1].
#' @param input_gamma Scalar. Lookaround horizon bound in [0, 1).
#' @param bidirectional Logical. Defaults to `FALSE`, which means that only
#'     the `from-to` relationship gets updated. If set to `TRUE`, this function
#'     will also update the `to-from` relationship.
#' @param edge_col_name The name of the column encoding the relation between
#'     two given nodes. By default, this is set to `successor_value`.
#' @return A tibble with NxN observations, such that every row is a pairwise
#'     combination of two nodes (`from` and `to`). Includes a column encoding
#'     the successor value between each pair of nodes.
#'
#' @export
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' karate_graph <- successr::karate %>%
#'     tidygraph::as_tbl_graph(directed = F)
#' karate_walk <- karate_graph %>%
#'     generate_random_walk(1000)
#' karate_graph %>%
#'     initialize_successor() %>%
#'     learn_from_observations(karate_walk, 0.1, 0.4, TRUE)
#'
learn_from_observations <- function(successor_matrix,
                                    observations,
                                    input_alpha, input_gamma,
                                    bidirectional = FALSE,
                                    edge_col_name = "successor_value") {

  obs_matrix <- as.matrix(select(observations, from, to))

  for (j in 1:nrow(observations)) {

    previous_state <- obs_matrix[j, 1]
    current_state <- obs_matrix[j, 2]

    successor_matrix <- successor_matrix %>%
      update_successor(input_alpha, input_gamma,
                       previous_state, current_state,
                       bidirectional)
  }

  output <- successor_matrix %>%
    matrix_to_adjlist(edge_col_name) %>%
    mutate(alpha = input_alpha,
           gamma = input_gamma)

  return (output)
}

