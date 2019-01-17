#' Add an edge between nodes in a graph object
#'
#' With a graph object of class \code{dgr_graph}, add an edge to nodes within
#'   the graph.
#' @inheritParams node_edge_aes_data
#' @inheritParams render_graph
#' @param from the outgoing node from which the edge is connected. There is the
#'   option to use a node \code{label} value here (and this must correspondingly
#'   also be done for the \code{to} argument) for defining node connections.
#'   Note that this is only possible if all nodes have distinct \code{label}
#'   values set and none exist as an empty string.
#' @param to the incoming nodes to which each edge is connected. There is the
#'   option to use a node \code{label} value here (and this must correspondingly
#'   also be done for the \code{from} argument) for defining node connections.
#'   Note that this is only possible if all nodes have distinct \code{label}
#'   values set and none exist as an empty string.
#' @param rel an optional string specifying the relationship between the
#'   connected nodes.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 4 nodes
#' graph <-
#'   create_graph() %>%
#'   add_node(label = "one") %>%
#'   add_node(label = "two") %>%
#'   add_node(label = "three") %>%
#'   add_node(label = "four")
#'
#' # Add an edge between those
#' # nodes and attach a
#' # relationship to the edge
#' graph <-
#'  add_edge(
#'    graph,
#'    from = 1,
#'    to = 2,
#'    rel = "A")
#'
#' # Use the `get_edge_info()`
#' # function to verify that
#' # the edge has been created
#' graph %>%
#'   get_edge_info()
#'
#' # Add another node and
#' # edge to the graph
#' graph <-
#'   graph %>%
#'   add_edge(
#'     from = 3,
#'     to = 2,
#'     rel = "A")
#'
#' # Verify that the edge
#' # has been created by
#' # counting graph edges
#' graph %>% count_edges()
#'
#' # Add edges by specifying
#' # node `label` values; note
#' # that all nodes must have
#' # unique `label` values to
#' # use this option
#' graph <-
#'   graph %>%
#'   add_edge(
#'     from = "three",
#'     to = "four",
#'     rel = "L") %>%
#'   add_edge(
#'     from = "four",
#'     to = "one",
#'     rel = "L")
#'
#' # Use `get_edges()` to verify
#' # that the edges were added
#' graph %>% get_edges()
#'
#' # Add edge aesthetic and data
#' # attributes during edge creation
#' graph_2 <-
#'   create_graph() %>%
#'   add_n_nodes(n = 2) %>%
#'   add_edge(
#'     from = 1,
#'     to = 2,
#'     rel = "M",
#'     edge_aes = edge_aes(
#'       penwidth = 1.5,
#'       color = "blue"),
#'     edge_data = edge_data(
#'       value = 4.3))
#'
#' # Use the `get_edges()` function
#' # to verify that the attribute
#' # values were bound to the
#' # newly created edge
#' graph_2 %>% get_edge_df()
#' @export
dag_edge <- function(graph,
                     from,
                     to) {
  ## get label from description if not specified in id
  if(!(from %in% graph$nodes_df$label)){
    from = graph$nodes_df$label[which(graph$nodes_df$description == from)]
  }
  if(!(to %in% graph$nodes_df$label)){
    to = graph$nodes_df$label[which(graph$nodes_df$description == from)]
  }

  graph = graph %>% DiagrammeR::add_edge(from,to)
    graph
  }


