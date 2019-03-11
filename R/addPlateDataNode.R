#' @importFrom rlang is_empty UQ enexpr enquo expr_text
### function to take a graph and a plate
### index and add a data node
addPlateDataNode = function(graph,plateIndex,rhs = NA) {

  plateIndexPosition = which(graph$plate_index_df$indexID == plateIndex)
  nodesOnThisPlate = graph$plate_node_df$nodeID[graph$plate_node_df$indexID == plateIndex]  ### used as to not create extract edges or dimensions for these nodes
  descr = graph$plate_index_df$indexDescription[plateIndexPosition]
  label = graph$plate_index_df$indexLabel[plateIndexPosition]
  dataString = graph$plate_index_df$dataNode[plateIndexPosition]

  ## rhs used for adding distribution to observed node
  ## for plates, the observed node is added automatically
  rhsExpr = rlang::enexpr(rhs) ##distribution or formula

  graph = graph %>%
    dag_node(descr = descr,
             label = label,
             rhs = !!rhsExpr)

  ## replace data node for newly created node with value of dataString
  ## I could not figure oute how to pass data as an argument in dag_node
  ## so this is a workaround
  newNodeIndex = which(graph$nodes_df$label == label)
  graph$nodes_df$data[newNodeIndex] = dataString
  graph$nodes_df$obs[newNodeIndex] = TRUE

  ## find all nodes in plate
  ## get parent node labels
  parentNodeLabels = graph$plate_node_df %>%
    dplyr::filter(indexID == plateIndex) %>% ##node on plate
    dplyr::left_join(graph$nodes_df, by = c("nodeID" = "id")) %>%
    dplyr::pull(label)

  ###add argDimLabels to children of parents that are not on this plate
  graph$arg_df = graph$plate_node_df %>%
    dplyr::filter(indexID == plateIndex) %>% ##node on plate
    dplyr::left_join(graph$edges_df, by = c("nodeID" = "from")) %>% ##chilren of nodes on plate
    dplyr::filter(!(to %in% nodesOnThisPlate)) %>% # get rid of same plate children
    dplyr::left_join(graph$nodes_df, by = c("to" = "id")) %>% # child node info
    dplyr::select(id,rhs,rhsID,parentNodeID = nodeID) %>%
    dplyr::left_join(graph$arg_df, by  = "rhsID") %>%
    dplyr::filter(argName %in% parentNodeLabels) %>%
    dplyr::select(argName,rhsID) %>%
    dplyr::mutate(newArgDim = label) %>%
    dplyr::right_join(graph$arg_df, by = c("argName", "rhsID")) %>%
    dplyr::group_by(rhsID,argName,argType,argValue,argDimLabels) %>%
    dplyr::summarize(argDimLabelCollapse = paste0(newArgDim, collapse = ",")) %>%
    dplyr::mutate(argDimLabels = ifelse(argDimLabelCollapse=="NA",as.character(NA),argDimLabelCollapse)) %>%
    dplyr::select(-argDimLabelCollapse) %>%
    as.data.frame()

  ### get all the children for the newly created data node
  childrenDF = graph$plate_node_df %>%
    dplyr::filter(indexID == plateIndex) %>% ## parent node on plate
    dplyr::left_join(graph$edges_df, by = c("nodeID" = "from")) ##chilren of nodes on plate

  ### make edges from new node to existing children
  childrenNewEdgeDF = childrenDF %>%
    dplyr::select(childID = to) %>%
    dplyr::mutate(parentNodeID = newNodeIndex) %>%
    dplyr::distinct()

  ### add new data node to plate of its children
  childPlatesDF = childrenNewEdgeDF %>% #get plates of children
    dplyr::filter(!(childID %in% nodesOnThisPlate)) %>% # get rid of same plate children
    dplyr::left_join(graph$plate_node_df, by = c("childID" = "nodeID"))

  ## update plate node df with parent Node ID
  ## and create edge from child to parent
  for (i in 1:NROW(childPlatesDF)) {
    graph$plate_node_df = dplyr::add_row(graph$plate_node_df,
                                         indexID = childPlatesDF$indexID[i],
                                         nodeID = childPlatesDF$parentNodeID[i])
  }
  # create edges
  graph = graph %>% dag_edge(from = childrenNewEdgeDF$parentNodeID,
                             to = childrenNewEdgeDF$childID)

  return(graph)

}
