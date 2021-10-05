#' @title Learn successor values from observations
#' @name learn_from_observations
#'
#' @description
#' After observing relations between nodes (i.e., transitioning between
#' states), update the successor values. This function learns from many
#' observations. To update the successor values from a single observation,
#' use `update_successor` instead.
#'
#' @param successor_matrix A square matrix created by `initialize_successor`.
#' @param observations A tibble with observations `from` a node `to` another.
#' @param alpha Scalar corresponding to the learning rate bound in [0, 1].
#' @param gamma Scalar corresponding to the lookaround horizon bound in [0, 1).
#' @param bidirectional Logical. Defaults to `FALSE`, which means that only
#'     the `from-to` relationship gets updated. If set to `TRUE`, this function
#'     will also update the `to-from` relationship.
#' @return A matrix with updated successor values, given the observations.
#'
#' @import dplyr
#' @import tidyr
#' @import igraph
#' @import tidygraph
#' @importFrom purrr pluck
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
#'     learn_from_observations(karate_walk, 0.1, 0.4, TRUE) %>%
#'     matrix_to_adjlist()
#'
learn_from_observations <- function(successor_matrix,
                                    observations,
                                    alpha, gamma,
                                    bidirectional = FALSE) {

  for (j in 1:nrow(observations)) {
    previous_state <- observations %>% slice(j) %>% pluck("from")
    current_state <- observations %>% slice(j) %>% pluck("to")

    successor_matrix <- successor_matrix %>%
      update_successor(alpha, gamma,
                       previous_state, current_state,
                       bidirectional)
  }

  return (successor_matrix)
}

