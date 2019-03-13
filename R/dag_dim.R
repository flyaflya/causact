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
dag_dim <- function(graph, ...) {

  ###retrieve nodeDF,edgeDF,argDF,plateIndexDF, and plateNodeDF
  nodeDF = graph$nodes_df
  edgeDF = graph$edges_df
  argDF = graph$arg_df
  plateDF = graph$plate_index_df
  plateNodeDF = graph$plate_node_df
  ### reset dimDF and recalculate it every time
  dimDF = data.frame(
      dimID = as.integer(NA),
      nodeID = as.integer(NA),
      dimType = as.character(NA),
      dimDataSource = as.character(NA),
      dimValue = as.integer(NA),
      stringsAsFactors = FALSE
    )[-1, ]


  # add dimID to nodeDF
  dimID = max(dimDF$dimID,0) + 1
  nodeDF$dimID = dimID:(dimID+nrow(nodeDF)-1)

  # make simple igraph to use topological sort
  # assign dimensions going bottom-up
  nodeIDOrder = igraph::graph_from_data_frame(edgeDF %>% select(from,to)) %>%
    igraph::topo_sort(mode = "in") %>%
    as.vector()

  ## append non-connected nodes into nodeIDOrder
  nodeIDOrder = union(nodeIDOrder,nodeDF$id)

  ### loop through nodes and add dimension one at a time
  for (nodeIDX in nodeIDOrder) {
    nodePosition = which(nodeDF$id == nodeIDX)
    dimIdOfNode = nodeDF$dimID[nodePosition]
    dataValOfNode = nodeDF$data[nodePosition]

    if(!is.na(dataValOfNode)){
      dimDF = dimDF %>%
        dplyr::add_row(dimID = dimID,
                       nodeID = nodeIDX,
                       dimType = "row",
                       dimDataSource = dataValOfNode,
                       dimValue = getRowDim(dataValOfNode,4)) %>%
        dplyr::add_row(dimID = dimID + 1,
                       nodeID = nodeIDX,
                       dimType = "column",
                       dimDataSource = dataValOfNode,
                       dimValue = getColDim(dataValOfNode,4))
    } else {
      dimDF = dimDF %>%
        dplyr::add_row(dimID = dimID,
                       nodeID = nodeIDX,
                       dimType = "row",
                       dimDataSource = as.character(NA),
                       dimValue = 1) %>%
        dplyr::add_row(dimID = dimID + 1,
                       nodeID = nodeIDX,
                       dimType = "column",
                       dimDataSource = as.character(NA),
                       dimValue = 1)
    }  # end if-else for row column type dimension

    # update counter for current dimID
    dimID = max(dimDF$dimID,0) + 1

    ## add plate indexes as needed
    combinedPlateDF = plateNodeDF %>%
      dplyr::filter(nodeID == nodeIDX) %>%
      dplyr::left_join(plateDF, by = "indexID") %>%
      dplyr::filter(!is.na(dataNode)) %>%
      dplyr::select(indexLabel,dataNode)

    if (nrow(combinedPlateDF) > 0) {
      dimDF = dimDF %>%
        dplyr::add_row(
          dimID = dimID:(dimID + nrow(combinedPlateDF) - 1),
          nodeID = nodeIDX,
          dimType = "plate",
          dimDataSource = combinedPlateDF$dataNode,
          dimValue = purrr::map_int(combinedPlateDF$dataNode, getPlateDim)
        )
      }
    # update counter for current dimID
    dimID = max(dimDF$dimID,0) + 1

  } ## end for loop

  ##add DIMENSION label to plate notation
  plateDimVector = ifelse(is.na(plateDF$dataNode),NA,
                          purrr::map_int(plateDF$dataNode, getPlateDim))
  plateDimLabel = ifelse(is.na(plateDimVector),"",
                         paste0(" [",plateDimVector,"]"))
  plateDF$indexDisplayName = paste0(plateDF$indexDisplayName,plateDimLabel)

  ## ASSUME all formula-node parents that are not rhs arguments
  ## are used for extraction of proper parameters
  # argEdgeList = nodeDF %>%
  #   filter(!is.na(rhs) & distr == FALSE) %>%
  #   select(id,label,rhsID) %>%
  #   left_join(argDF, by = "rhsID") %>%
  #   mutate(argNodeID = findNodeID(nodeDF,argName)) %>%
  #   rename(to = id, from = argNodeID)
  #
  # extractionEdges = edgeDF %>% anti_join(argEdgeList, by = c("from", "to")) %>%
  #   filter(to %in% argEdgeList$to) %>%
  #   pull(id) ###edge id's to change to dashed edge
  #
  # edgeDF$type[extractionEdges] = "extract"

  # update graph


    graph$nodes_df = nodeDF
    graph$edges_df = edgeDF
    graph$plate_index_df = plateDF
    graph$plate_node_df = plateNodeDF
    graph$dim_df = dimDF

  return(graph)  ## return updated graph
}
