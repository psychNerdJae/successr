#' @title Convert between network formats
#'
#' @description
#' Different network formats are useful for different operations of interest.
#' For example, successor algorithms rely on matrix operations, but it is often
#' useful to convert matrices into tidy dataframes for visualization (i.e.,
#' using `ggplot2`). Similarly, when simulating the dynamics of a given network,
#' it can be useful to maintain a graph (i.e., using `tidygraph`). This family
#' of functions enables straightforward (but non-exhaustive) conversion between
#' formats, with a focus on getting to two "target" formats.
#'
#' The first format is a matrix, because all successor updates must occur in a
#' matrix. This motivates `convert_graph_to_matrix` and
#' `convert_adjlist_to_matrix`.
#'
#' The second format is something I call an "adjlist", which is basically a
#' matrix in a tidy dataframe. The column `from` indexes rows, the column `to`
#' indexes columns, and the column `edge` encodes the connection strength. This
#' blends together the basic format of edgelists, but maintains an explicit
#' record of all possible relations (i.e., such that there are as many dataframe
#' observations as there are matrix cells). This tidy format is useful for
#' plotting, and motivates `convert_graph_to_adjlist` and
#' `convert_matrix_to_adjlist`.
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' # Create a graph, then convert into matrix
#' successr::karate %>%
#'   tidygraph::tbl_graph(edges = ., directed = F) %>%
#'   successr::convert_graph_to_matrix()
#'
#' @param input Depending on the function, a permissible input could be a
#' matrix, dataframe/tibble, or `tidygraph::tbl_graph`.
#' @param relation_value_col The name of the column that encodes (or will
#' encode) the strength of relationship between two nodes. For example, in an
#' adjacency representation, this might be zeroes (not friends) and ones
#' (yes friends).
#'
#' @export
convert_graph_to_matrix <- function(input) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop(
      "Package \"tidygraph\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (all(class(input) != "tbl_graph")) {
    stop("Input is not `tbl_graph`")
  }

  return (
    input %>%
      convert_graph_to_adjlist(edge) %>%
      convert_adjlist_to_matrix(edge)
  )
}

#' @export
#' @rdname convert_graph_to_matrix
convert_graph_to_adjlist <- function(input, relation_value_col) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop(
      "Package \"tidygraph\" must be installed to use this function.",
      call. = FALSE
    )
  }

  if (all(class(input) != "tbl_graph")) {
    stop("Input is not `tbl_graph`")
  }

  # Check whether the graph is directed or undirected
  directed_graph <- tidygraph::with_graph(
    input,
    tidygraph::graph_is_directed()
  )

  # Get the edgelist
  edgelist <- input %>%
    tidygraph::activate("edges") %>%
    tidygraph::as_tibble() %>%
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
  n_nodes <- igraph::vcount(input)
  output <- expand_grid(
    from = 1:n_nodes,
    to = 1:n_nodes
  ) %>%
    # Add info about what edges exist in the network
    left_join(edgelist, by = c("from", "to")) %>%
    # Mark "missing" edges as non-existent edges
    mutate(edge = replace_na(edge, 0)) %>%
    # Rename the "edge" column whatever the user wants
    rename({{relation_value_col}} := edge)

  return (output)
}

#' @export
#' @rdname convert_graph_to_matrix
convert_adjlist_to_matrix <- function(input, relation_value_col) {
  if (!any(class(input) %in% class(tibble()))) {
    stop("Input is not `data.frame`-like")
  }

  these_cols <- colnames(input)
  if (!any(these_cols == "from") | !any(these_cols == "to")) {
    stop("Your dataframe does not contain the columns `from` and `to`.")
  }

  if (!is.numeric(input$from) | !is.numeric(input$to)) {
    stop("One or both of the columns `from` and `to` contain non-numbers.")
  }

  if (!any(these_cols == user_col_to_string({{relation_value_col}}))) {
    stop("Your dataframe does not contain the specified relation value column.")
  }

  if (
    input %>%
    select({{relation_value_col}}) %>%
    tibble::deframe() %>%
    class()
    != "numeric"
  ) {
    stop("Your relation value column contains non-numbers.")
  }

  n_nodes <- with(input, max(from, to))
  if (nrow(input) != n_nodes*(n_nodes-1) + n_nodes) {
    # adjlist MUST include identity (from == to)
    stop("You have the wrong number of observations in your adjlist.")
  }

  return (
    input %>%
      select(from, to, {{relation_value_col}}) %>%
      arrange(from, to) %>%
      pivot_wider(
        names_from = to,
        values_from = {{relation_value_col}}
      ) %>%
      select(-from) %>%
      as.matrix() %>%
      unname()
  )
}

#' @export
#' @rdname convert_graph_to_matrix
convert_matrix_to_adjlist <- function(input, relation_value_col) {

  if (!any(class(input) == "matrix")) {
    stop("Input is not a matrix.")
  }

  return (
    input %>%
      as.data.frame() %>%
      mutate(from = row_number()) %>%
      pivot_longer(
        cols = -from,
        names_to = "to",
        values_to = user_col_to_string({{relation_value_col}})
      ) %>%
      mutate(
        to = stringr::str_remove(to, "V"),
        to = as.numeric(to)
      )
  )
}
