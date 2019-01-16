#' Set node attribute values to reflect a desired cluster
#'
#' From a graph object of class \code{dgr_graph}, set node attribute values for
#'   one or more nodes.
#' @inheritParams render_graph
#' @param node_attr the name of the attribute to set.
#' @param values the values to be set for the chosen attribute for the chosen
#'   nodes.
#' @param nodes an optional vector of node IDs for filtering the list of nodes
#'   present in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Set attribute `color = "green"` for
#' # nodes `1` and `3` using the graph object
#' graph <-
#'   graph %>%
#'   set_node_attrs(
#'     node_attr = color,
#'     values = "green",
#'     nodes = c(1, 3))
#'
#' # View the graph's node data frame
#' graph %>% get_node_df()
#'
#' # Set attribute `color = "blue"` for
#' # all nodes in the graph
#' graph <-
#'   graph %>%
#'   set_node_attrs(
#'     node_attr = color,
#'     values = "blue")
#'
#' # Display the graph's ndf
#' graph %>% get_node_df()
#' @importFrom dplyr mutate
#' @importFrom rlang enquo get_expr UQ

#' @export
dag_plate <- function(graph,
                      indexLabel,
                      description,
                      nodeLabels) {

  ## get selection of node IDS from labels
  nodeIDS = dplyr::left_join(
    data.frame(label = nodeLabels, stringsAsFactors = FALSE),graph$nodes_df) %>%
    .$id

  ## update plate index DF
  lastPlateIndex = max(graph$plate_index_df$indexID,0)
  graph$plate_index_df = dplyr::add_row(graph$plate_index_df,
                                        indexID = lastPlateIndex + 1,
                                        indexLabel = indexLabel,
                                        indexDescription = description,
                                        indexDisplayName = paste0(description," ",indexLabel))

  ## update plate node df
  for (i in seq_along(nodeIDS)){
    graph$plate_nodes_df = dplyr::add_row(graph$plate_nodes_df,
                                          indexID = lastPlateIndex + 1,
                                          nodeID = nodeIDS[i])
  }

  graph  ## return updated graph
}
