#' Add a node to an existing graph object
#'
#' With a graph object of class \code{dgr_graph}, add a new node to the graph.
#'   One can optionally provide node attributes for the created node. There is
#'   also the option to create edges to and from existing nodes in the graph.
#'   Because new edges can also be created through this function, there is the
#'   possibility to set edge attributes for any new graph edges.
#' @inheritParams node_edge_aes_data
#' @inheritParams render_graph
#' @param type an optional character object that acts as a group identifier for
#'   the node to be added.
#' @param label an optional character object that describes the node.
#' @param from an optional vector containing node IDs from which edges will be
#'   directed to the new node.
#' @param to an optional vector containing node IDs to which edges will be
#'   directed from the new node.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph and add 2 nodes by using
#' # the `dag_node()` function twice
#' graph <-
#'   create_graph() %>%
#'   dag_node() %>%
#'   dag_node()
#'
#' # Get a count of all nodes
#' # in the graph
#' graph %>% count_nodes()
#'
#' # The nodes added were given
#' # ID values `1` and `2`; obtain
#' # the graph's node IDs
#' graph %>% get_node_ids()
#'
#' # Add a node with a `type`
#' # value defined
#' graph <-
#'   graph %>%
#'   dag_node(type = "person")
#'
#' # View the graph's internal
#' # node data frame (ndf)
#' graph %>% get_node_df()
#' @importFrom dplyr select bind_cols as_tibble
#' @importFrom rlang is_empty UQ enexpr
#' @export
dag_node <- function(graph,
                     label,
                     description = as.character(NA),
                     distr = greta::variable,
                     observed = FALSE,
                     formulaString = as.character(NA),
                     children = as.character(NA)) {

  # update node_data with DAG specific graphing
  description = description
  distr = rlang::enexpr(distr)  ## take in argument as expression
  type = ifelse(observed == TRUE,"obs","latent")
  formulaString = formulaString

  # get node labels based off of user input for distr
  distList = getFullDistList(rlang::UQ(distr))

  ## above returns list of dist name, dist arguments, and label
  distString = distList$distString
  fullDistLabel = distList$fullDistLabel

  ##use formula string for label if available
  ##distribution is ignored when formulaString is provided
  if (!is.na(formulaString)) {
    fullDistLabel = formulaString
  }

  graph = graph %>% DiagrammeR::add_node(
    type = type,
    label = label,
    node_data =
      DiagrammeR::node_data(
        type = type,
        description = description,
        distr = distString,
        fullDistLabel = fullDistLabel,
        formulaString = formulaString),
    node_aes =
      DiagrammeR::node_aes(peripheries = ifelse(is.na(formulaString), 1, 2))
  )

  # Add child edge if desired
  if (!is.na(children)) {
    graph = graph %>% dag_edge(from = label, to = children)
  }


  return(graph)
}
