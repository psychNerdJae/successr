#' @title Learn successor values from observations (many parameters)
#' @name learn_from_param_sweep
#'
#' @description
#' After observing relations between nodes (i.e., transitioning between
#' states), update the successor values. This function learns from many
#' observations, and updates according to many parameter values. To update the
#' successor values from a single observation, use `update_successor` instead.
#' To update successor values from a single set of parameters, use
#' `learn_from_observations` instead.
#'
#' @param successor_matrix A square matrix created by `initialize_successor`.
#' @param observations A tibble with observations `from` a node `to` another.
#' @param alphas Vector corresponding to the learning rate bound in [0, 1].
#' @param gammas Vector corresponding to the lookaround horizon bound in [0, 1).
#' @param bidirectional Logical. Defaults to `FALSE`, which means that only
#'     the `from-to` relationship gets updated. If set to `TRUE`, this function
#'     will also update the `to-from` relationship.
#' @return A dataframe with updated successor values, given the observations.
#'
#' @import dplyr
#' @import tidyr
#' @import igraph
#' @import tidygraph
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
#'     learn_from_param_sweep(karate_walk, c(0.1, 0.2), c(0, 0.4), TRUE)
#'
learn_from_param_sweep <- function(successor_matrix,
                                   observations,
                                   alphas, gammas,
                                   bidirectional = FALSE) {

  for (this_alpha in alphas) {
    for (this_gamma in gammas) {
      # Update successor values for one combination of parameter values
      this_sr <- successor_matrix %>%
        learn_from_observations(observations,
                                this_alpha, this_gamma,
                                bidirectional) %>%
        matrix_to_adjlist() %>%
        mutate(alpha = this_alpha,
               gamma = this_gamma)

      # Return one giant dataframe for all combinations of parameter values
      if (exists("output") && is.data.frame(get("output"))) {
        output <- add_row(output, this_sr)
      } else {
        output <- this_sr
      }
    }
  }

  return (output)
}

