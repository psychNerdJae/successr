#' @title Convert graph into an adjacency list
#' @name graph_to_adjlist
#'
#' @description
#' An "adjacency list" is similar to an edgelist, in that every row encodes the
#' relationship between two nodes. However, an edgelist will typically contain
#' only non-zero relationships.
#'
#' On the other hand, in an adjlist, we are explicitly interested in maintaining
#' a full list of all possible pairwise relationships.
#'
#' Beware combinatorical explosion for large graphs.
#'
#' @param input_tidygraph A `tbl_graph` representation from `tidygraph`.
#'     This function will not work with other kinds of network representations,
#'     e.g. from `igraph`. Make sure that your input graph is either directed
#'     or undirected, as you'd like it.
#' @return A tibble containing as many observations as there are cells in an
#'     NxN matrix, where N=number of nodes.
#'
#' @import dplyr
#' @import tidyr
#' @import igraph
#' @import tidygraph
#' @export
#'
#' @examples
#' karate <- tidygraph::as_tbl_graph(successr::karate, directed = F)
#' graph_to_adjlist(karate)
#'
graph_to_adjlist <- function(input_tidygraph) {

  # Check whether the graph is directed or undirected
  directed_graph <- with_graph(
    input_tidygraph,
    graph_is_directed()
  )

  # Get the edgelist
  edgelist <- input_tidygraph %>%
    activate("edges") %>%
    as_tibble() %>%
    select(from, to) %>%
    mutate(edge = 1)

  # Make symmetric if network is undirected
  if (!directed_graph) {
    edgelist <- bind_rows(
      # Original
      edgelist,
      # Flipped
      edgelist %>% select(from = to, to = from, edge)
    )
  }

  # Create a dataframe with all possible pairwise relations
  output_df <- expand_grid(from = 1:vcount(input_tidygraph),
                           to = 1:vcount(input_tidygraph)) %>%
    # Add info about what edges exist in the network
    left_join(edgelist, by = c("from", "to")) %>%
    # Mark "missing" edges as non-existent edges
    mutate(edge = replace_na(edge, 0))

  return (output_df)
}

