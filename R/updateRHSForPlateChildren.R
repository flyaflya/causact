### update rhs for children not on plate
updateRHSForPlateChildren = function(graph, plateIndex) {
  nodesOnThisPlate = graph$plate_node_df$nodeID[graph$plate_node_df$indexID == plateIndex]  ### used as to not create extract edges or dimensions for these nodes
  label = graph$plate_index_df$indexLabel[graph$plate_index_df$indexID == plateIndex]

  ## find all nodes in plate
  ## get parent node labels
  parentNodeLabels = graph$plate_node_df %>%
    dplyr::filter(indexID == plateIndex) %>% ##node on plate
    dplyr::left_join(graph$nodes_df, by = c("nodeID" = "id")) %>%
    dplyr::pull(label)

  ###add argDimLabels to children that are not on this plate
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


  return(graph)

}

