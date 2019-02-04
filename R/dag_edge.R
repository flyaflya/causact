#' Add an edge between nodes in a graph object.
#'
#' With a graph object of class \code{dgr_graph} created from \code{dag_create}, add an edge between nodes in the graph.
#' @param from the parent node label or description from which the edge is connected.
#' @param to the child node label or description from which the edge is connected.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 2 connected nodes
#' dag_create() %>%
#'   dag_node("X") %>%
#'   dag_node("Y") %>%
#'   dag_edge(from = "X", to = "Y") %>%
#'   dag_render(shortLabel = TRUE)
#' @importFrom DiagrammeR add_edge
#' @export
dag_edge <- function(graph,
                     from,
                     to) {
  ## get label from description if not specified in id
  for (i in seq_along(from)) {
    if (!(from[i] %in% graph$nodes_df$label)) {
      from[i] = graph$nodes_df$label[which(graph$nodes_df$description == from)]
    }
  }

  for (i in seq_along(to)) {
    if (!(to %in% graph$nodes_df$label)) {
      to[i] = graph$nodes_df$label[which(graph$nodes_df$description == to)]
    }
  }

  graph = graph %>% DiagrammeR::add_edge(from, to)
  return(graph)
}


