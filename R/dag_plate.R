#' Set node attribute values to reflect a desired cluster
#'
#' From a graph object of class \code{dgr_graph}, set node attribute values for
#'   one or more nodes.
#' @param graph a graph object of class \code{dgr_graph} created using \code{dag_create()}.
#' @param descr a longer more descriptive label for the cluster/plate.
#' @param label a short character string to use as an index letter
#'   nodes.
#' @param nodeLabels a vector of node labels or descriptions to include in the list of nodes
#' @param data a vector representing the categorical data whose unique values become the plate index.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # create small DF
#' dag_create() %>%
#'     dag_node("Y","House Prices") %>%
#'     dag_plate("i","Observation",nodeLabels = "Y") %>%
#'     dag_render(shortLabel = TRUE)
#' @importFrom dplyr mutate
#' @importFrom rlang enquo get_expr UQ
#' @export

dag_plate <- function(graph,
                      descr,
                      label,
                      nodeLabels,
                      data = as.character(NA),
                      addDataNode = FALSE,
                      rhs = NA ) {

  ### capture data argument as string
  dataNodeExpr = rlang::enexpr(data)
  dataNodeString = rlang::expr_text(dataNodeExpr)
  if(is.na(data[1])) {dataNodeString = as.character(NA)}

  ## rhs used for adding distribution to observed node
  ## for plates, the observed node is added automatically
  rhsExpr = rlang::enexpr(rhs) ##distribution or formula

  ### get nodeIDS for enterned node labels
  ### node labels can be labels, descr, or data
  nodeIDS = findNodeID(graph,nodeLabels)

  ## update plate index DF
  lastPlateIndex = max(graph$plate_index_df$indexID, 0)
  graph$plate_index_df = dplyr::add_row(
    graph$plate_index_df,
    indexID = lastPlateIndex + 1,
    indexLabel = label,
    indexDescription = descr,
    indexDisplayName = paste0(descr, " ", label),
    dataNode = dataNodeString
  )

  ## update plate node df
  for (i in seq_along(nodeIDS)) {
    graph$plate_node_df = dplyr::add_row(graph$plate_node_df,
                                          indexID = lastPlateIndex + 1,
                                          nodeID = nodeIDS[i])
  }

  ##

  ## add data node to extract the proper parameter
  if (!is.na(dataNodeString) & addDataNode == TRUE) {
    graph = graph %>% addPlateDataNode(plateIndex = lastPlateIndex + 1,
                                       rhs = !!rhsExpr)
  } else {
    ## add indexing to children if dataNodeString is NA
    graph = graph %>%
      updateRHSForPlateChildren(plateIndex = lastPlateIndex + 1)
  }

  return(graph)  ## return updated graph
}
