## function to convert graph to Diagrammer object
## for visualization
dag_diagrammer = function(graph, wrapWidth = 24, ...) {
  # add dimension labels
  graph = graph %>% dag_dim()

  # create new graph
  dgr_graph = DiagrammeR::create_graph(directed = TRUE)



  ###retrieve nodeDF,edgeDF,argDF,plateIndexDF, and plateNodeDF
  nodeDF = graph$nodes_df
  edgeDF = graph$edges_df
  argDF = graph$arg_df
  plateDF = graph$plate_index_df
  plateNodeDF = graph$plate_node_df
  dimDF = graph$dim_df



  ### line needed because DiagrammeR does not support
  ### nested or intersecting subgraphs
  if(nrow(graph$plate_index_df)>1) {
    graph$plate_index_df = plateDF
    graph = graph %>% pseudoPlate()
    plateDF = graph$plate_index_df
  }



  ###make the top line equal to auto_descr (for now ... going with standard output) ... there will always be at least one node
  nodeDF$descLine = ifelse(is.na(nodeDF$auto_descr),
                           nodeDF$auto_label,
                           nodeDF$auto_descr) %>%
                    abbreviate(minlength = wrapWidth)

  ###section for adding dimension to description
  dimLabelDF = dimDF %>%
    dplyr::filter(dimValue > 1 & dimType != "plate") %>%
    dplyr::group_by(nodeID) %>%
    dplyr::summarize(dimLabel = paste0(dimValue, collapse = "\U00D7")) %>%
    dplyr::mutate(dimLabel = paste0(" [",dimLabel,"]"))

  nodeDF = nodeDF %>%
    dplyr::left_join(dimLabelDF,by = c("id" = "nodeID")) %>%
    dplyr::mutate(dimLabel = tidyr::replace_na(dimLabel,"")) %>%
    dplyr::mutate(descLine = paste0(descLine,dimLabel))

  ###make the equation line nicely formatted
  ###equation may not be specified
  ###first collapse arg_df, then use in equation line
  eqLineDF = argDF %>%
    dplyr::filter(argType == "param" |
             (!is.na(argValue) & argType == "arg")) %>%
    dplyr::mutate(textToCollapse = ifelse(is.na(argValue), argName, argValue)) %>%
    dplyr::mutate(textToCollapse = ifelse(
      argType == "arg",
      paste0(argName, "=", argValue),
      textToCollapse
    )) %>%
    dplyr::group_by(rhsID) %>%
    dplyr::summarize(argList = paste0(textToCollapse, collapse = ","))

  ## create the equation line
  ## if rhs represents a formula, use "label = formula"
  ## if rhs represents distribution, use "label ~ distribution(args)
  ## if rhs is blank, use "label"
  eqDF = nodeDF %>%
    dplyr::select(id, auto_label, rhs, rhsID, distr) %>%
    dplyr::left_join(eqLineDF, by = "rhsID") %>%
    dplyr::mutate(eqLine = ifelse(!distr & !is.na(rhs), paste0(auto_label," = ",rhs),
                           ifelse(is.na(rhs), auto_label,
                                  paste0(auto_label, " ~ ", rhs, "(", argList, ")")))) %>%
    dplyr::select(id, eqLine)

  ### create nodeDF as DiagrammeR data frame
  ##create clusterNameDF to map nodes to plates
  clusterNameDF = plateNodeDF %>%
    dplyr::left_join(plateDF, by = "indexID") %>%
    dplyr::select(id = nodeID, cluster = indexDisplayName)

  nodeDF = nodeDF %>%
    dplyr::left_join(eqDF, by = "id") %>%
    dplyr::mutate(type = ifelse(obs == TRUE,"obs","latent"),
           peripheries = ifelse(distr == TRUE | is.na(rhs),1,2),
           fillcolor = ifelse(obs == TRUE,"cadetblue","aliceblue"),
           label = ifelse(descLine == eqLine,descLine,paste0(descLine,"\n",eqLine)) %>%  ###poor man's version of shortLabel
    dplyr::select(id,label,type,peripheries,fillcolor) %>%
    dplyr::left_join(clusterNameDF, by = "id")

  ### use DIagrammeR::create_node_df
  nodeDF = DiagrammeR::create_node_df(
    n = nrow(nodeDF),
    id = nodeDF$id,
    label = nodeDF$label,
    type = nodeDF$type,
    peripheries = nodeDF$peripheries,
    fillcolor = nodeDF$fillcolor,
    cluster = nodeDF$cluster
    )

  dgr_graph = DiagrammeR::create_graph(directed = TRUE) %>%
    DiagrammeR::add_node_df(nodeDF)

  ### add egdes if applicable
  if (nrow(edgeDF) > 0) {
    ## use dashed for type = extract
    edgeDF$style = ifelse(edgeDF$type == "extract","dashed","solid") %>%
      tidyr::replace_na("solid")
    edgeDF = DiagrammeR::create_edge_df(from = edgeDF$from,
                                        to = edgeDF$to,
                                        style = edgeDF$style)
    dgr_graph =  dgr_graph %>% DiagrammeR::add_edge_df(edgeDF)
  }

  ###update global graph attributes
  dgr_graph = dgr_graph %>% causact::setDirectedGraphTheme()

  return(dgr_graph)
}
