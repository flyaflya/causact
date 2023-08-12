#' Add dimension information to `causact_graph`
#'
#' Internal function that is used as part of rendering graph or running greta.
#' @param graph a graph object of class `causact_graph` created using `dag_create()`.
#' @return a graph object of class `causact_graph` with populated dimension information.
#' @importFrom dplyr mutate filter left_join select add_row
#' @importFrom purrr map_int
#' @importFrom igraph graph_from_data_frame topo_sort

dag_dim <- function(graph) {

  ## First validate that the first argument is indeed a causact_graph
  class_g <- class(graph)
  ## Any causact_graph will have class length of 1
  if(length(class_g) > 1){
    ## This specific case is hard-coded as it has occured often in early use by the author
    if(class_g[1] == chr("grViz") && class_g[2]=="htmlwidget"){
      errorMessage <- paste0("Given rendered Causact Graph. Check the declaration for a dag_render() call.")
    }
    else {
      errorMessage <- paste0("Cannot add dimension information to given object as it is not a Causact Graph.")
    }
    stop(errorMessage)
  }
  ## Now check the single class
  if(class_g != "causact_graph"){
    errorMessage <- paste0("Cannot add dimension information to given object as it is not a Causact Graph.")
    stop(errorMessage)
  }

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
      dimLabel = as.character(NA),
      stringsAsFactors = FALSE
    )[-1, ]


  # add dimID to nodeDF
  dimID = max(dimDF$dimID,0) + 1
  nodeDF$dimID = dimID:(dimID+nrow(nodeDF)-1)

  # make simple igraph to use topological sort
  # assign dimensions going bottom-up
  nodeIDOrder = igraph::graph_from_data_frame(edgeDF %>% dplyr::select(from,to)) %>%
    igraph::topo_sort(mode = "in") %>%
    names() %>%
    as.integer()

  ## append non-connected nodes into nodeIDOrder
  nodeIDOrder = union(nodeIDOrder,nodeDF$id)

  ### loop through nodes and add dimension one at a time
  for (nodeIDX in nodeIDOrder) {
    nodePosition = which(nodeDF$id == nodeIDX)
    dimIdOfNode = nodeDF$dimID[nodePosition]
    dataValOfNode = nodeDF$data[nodePosition]

    if(!is.na(dataValOfNode)){
      dimDF = dimDF %>%
        dplyr::add_row(dimID = dimIdOfNode,
                       nodeID = nodeIDX,
                       dimType = "row",
                       dimDataSource = dataValOfNode,
                       dimValue = getRowDim(dataValOfNode,4)) %>%
        dplyr::add_row(dimID = dimIdOfNode,
                       nodeID = nodeIDX,
                       dimType = "column",
                       dimDataSource = dataValOfNode,
                       dimValue = getColDim(dataValOfNode,4))
    } else {
      dimDF = dimDF %>%
        dplyr::add_row(dimID = dimIdOfNode,
                       nodeID = nodeIDX,
                       dimType = "row",
                       dimDataSource = as.character(NA),
                       dimValue = 1) %>%
        dplyr::add_row(dimID = dimIdOfNode,
                       nodeID = nodeIDX,
                       dimType = "column",
                       dimDataSource = as.character(NA),
                       dimValue = 1)
    }  # end if-else for row column type dimension

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
          dimValue = purrr::map_int(combinedPlateDF$dataNode, getPlateDim),
          dimLabel = combinedPlateDF$indexLabel
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

  ## update graph prior to looking for extraction edges
  graph$nodes_df = nodeDF
  graph$edges_df = edgeDF
  graph$plate_index_df = plateDF
  graph$plate_node_df = plateNodeDF
  graph$dim_df = dimDF

  ## ASSUME all edges that go from one plate to another
  ## are candidates to be extraction edges
  graph = updateExtractEdges(graph)
  graph = updateExtractArguments(graph)
  #
  # extractionEdges = edgeDF %>% anti_join(argEdgeList, by = c("from", "to")) %>%
  #   filter(to %in% argEdgeList$to) %>%
  #   pull(id) ###edge id's to change to dashed edge
  #
  # edgeDF$type[extractionEdges] = "extract"

  # update graph




  return(graph)  ## return updated graph
}
