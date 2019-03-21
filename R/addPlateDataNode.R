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

  ### get all the children for the newly created data node
  childrenDF = graph$plate_node_df %>%
    dplyr::filter(indexID == plateIndex) %>% ## parent node on plate
    dplyr::left_join(graph$edges_df, by = c("nodeID" = "from")) ##chilren of nodes on plate

  ### make edges from new node to existing children
  childrenNewEdgeDF = childrenDF %>%
    dplyr::select(childID = to) %>%
    dplyr::mutate(parentNodeID = newNodeIndex) %>%
    dplyr::distinct()

  ### add new data node to plate of its children (if on plate)
  childPlatesDF = childrenNewEdgeDF %>% #get plates of children
    dplyr::filter(!(childID %in% nodesOnThisPlate)) %>% # get rid of same plate children
    dplyr::left_join(graph$plate_node_df, by = c("childID" = "nodeID")) %>%
    dplyr::filter(!is.na(indexID))

  ## update plate node df with parent Node ID
  if(NROW(childPlatesDF) >0) {
  for (i in 1:NROW(childPlatesDF)) {
    graph$plate_node_df = dplyr::add_row(graph$plate_node_df,
                                         indexID = childPlatesDF$indexID[i],
                                         nodeID = childPlatesDF$parentNodeID[i])
  }}
  ## and create edge from child to parent
  # create edges
  if(NROW(childrenNewEdgeDF) >0) {
  graph = graph %>% dag_edge(from = childrenNewEdgeDF$parentNodeID,
                             to = childrenNewEdgeDF$childID)
  }

  return(graph)

}
