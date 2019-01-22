#' Set node attribute values to reflect a desired cluster
#'
#' From a graph object of class \code{dgr_graph}, set node attribute values for
#'   one or more nodes.
#' @param graph a graph object of class \code{dgr_graph} created using \code{dag_create()}.
#' @param indexLabel a short character string to use as an index letter
#'   nodes.
#' @param description a longer more descriptive label for the cluster/plate.
#' @param nodeLabels a vector of node labels or descriptions to include in the list of nodes
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
                      indexLabel,
                      description,
                      nodeLabels,
                      data = as.character(NA)) {
  ### capture data argument
  data = enquo(data)
  data = rlang::expr_text(data)

  ## get label from description if not available as id
  for (i in seq_along(nodeLabels)) {
    if (!(nodeLabels[i] %in% graph$nodes_df$label)) {
      indexOfDescription = which(graph$nodes_df$description == nodeLabels[i])
      nodeLabels[i] =
        graph$nodes_df$label[indexOfDescription]
    }
  }

  ## get selection of node IDS from labels
  nodeIDS = dplyr::left_join(data.frame(label = nodeLabels, stringsAsFactors = FALSE),
                             graph$nodes_df) %>%
    .$id

  ## update plate index DF
  lastPlateIndex = max(graph$plate_index_df$indexID, 0)
  graph$plate_index_df = dplyr::add_row(
    graph$plate_index_df,
    indexID = lastPlateIndex + 1,
    indexLabel = indexLabel,
    indexDescription = description,
    indexDisplayName = paste0(description, " ", indexLabel),
    data = data
  )

  ## update plate node df
  for (i in seq_along(nodeIDS)) {
    graph$plate_nodes_df = dplyr::add_row(graph$plate_nodes_df,
                                          indexID = lastPlateIndex + 1,
                                          nodeID = nodeIDS[i])
  }

  ###update graph cluster information for plates
  ##### Code to render plate notation subgraphs when
  ##### there are either intersecting or nested subgraphs

  pseudo_plate_index_df = graph$plate_index_df
  pseudo_plate_nodes_df = graph$plate_nodes_df


  ## is any node duplicated
  duplicateFlag = anyDuplicated(graph$plate_nodes_df$nodeID)

  ## if node appears twice, need to make a virtual cluster
  ## such that each node is a member of a unique subgraph
  ## composed of all of the node's indexes
  while (duplicateFlag > 0) {
    duplicatedNodeID = graph$plate_nodes_df$nodeID[duplicateFlag]

    ## get vector of indexID's for node
    indices = graph$plate_nodes_df$indexID[graph$plate_nodes_df$nodeID == duplicatedNodeID]

    ## make new combined plate for multi-index node
    newIndex = max(graph$plate_index_df$indexID) + 1
    newIndexLabel = paste(graph$plate_index_df$indexLabel[indices], collapse = "")
    newIndexDescr = paste(graph$plate_index_df$indexDescription[indices], collapse = "_")
    newIndexDispName = paste0(
      paste0(
        graph$plate_index_df$indexDescription[indices],
        " ",
        graph$plate_index_df$indexLabel[indices],
        collapse = "\\r"
      ),
      "\\r"
    )
    newRow = data.frame(
      indexID = newIndex,
      indexLabel = newIndexLabel,
      indexDescription = newIndexDescr,
      indexDisplayName = newIndexDispName,
      stringsAsFactors = FALSE
    )

    pseudo_plate_index_df = dplyr::bind_rows(graph$plate_index_df, newRow)

    ## make nodes point to new plate
    pseudo_plate_nodes_df = graph$plate_nodes_df %>%
      dplyr::filter(nodeID != duplicatedNodeID) %>%
      dplyr::bind_rows(data.frame(indexID = newIndex, nodeID = duplicatedNodeID))

    ## condition to break out.
    duplicateFlag = anyDuplicated(pseudo_plate_nodes_df$nodeID)
  }


  ## update attributes for plate notation - actual plate node and index df's are not updated  -- probably inefficient code, but working
  if (nrow(graph$plate_nodes_df) > 0) {
    plateDF = dplyr::left_join(pseudo_plate_nodes_df, pseudo_plate_index_df) %>%
      dplyr::mutate(clusterName = indexDisplayName) %>%
      dplyr::select(nodeID, clusterName) %>%
      dplyr::rename(id = nodeID)
    graph$nodes_df$cluster = dplyr::left_join(graph$nodes_df, plateDF) %>% .$clusterName
  }

  graph$nodes_df$fontcolor = "black"  ###fix for weird fontcolor bug

  graph  ## return updated graph
}
