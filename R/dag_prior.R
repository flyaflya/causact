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
#' @export
dag_prior <- function(graph,
                     childLabel,
                     parentArgName,
                     description = NULL,  ##this and below ignored right now
                     label = NULL,
                     type = "latent",
                     distribution = greta::variable,
                     from = NULL,
                     to = NULL,
                     node_aes = NULL,
                     edge_aes = NULL,
                     node_data = NULL,
                     edge_data = NULL,
                     peripheries = 1) {

  ####
  ### get node ID from label
  nodeID = graph$nodes_df$id[which(graph$nodes_df$label == childLabel)]

  ### give name to new node
  newNodeLabel = paste0(parentArgName,"_",childLabel)

  #### get number of parents
  #distribution = graph$nodes_df$distribution[nodeID]
  #distString = paste0("greta::",distribution)
  #distExpr = parse(text = distString)
  #distArgs = formalArgs(eval(distExpr))
  ###assume arguments before dim are required parameters
  #numParents = which(distArgs == "dim") - 1

  ### create one parent node for each argument
  # get number of arguments
  #distribution = tail(as.character(substitute(distribution)), n=1)

  ### create node with edge to child
  graph = graph %>% dag_node(graph = graph,description = description,label = newNodeLabel) %>% dag_edge(from = newNodeLabel, to = childLabel)

      return(graph)
    }

