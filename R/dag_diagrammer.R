## function to convert graph to Diagrammer object
## for visualization
dag_diagrammer = function(graph, wrapWidth = 24, ...) {
  dgr_graph = DiagrammeR::create_graph(directed = TRUE)

  ### line needed because DiagrammeR does not support
  ### nested or intersecting subgraphs
  if(nrow(graph$plate_index_df)>1) {graph = graph %>% pseudoPlate()}

  ###retrieve nodeDF,edgeDF,argDF,plateIndexDF, and plateNodeDF
  nodeDF = graph$nodes_df
  edgeDF = graph$edges_df
  argDF = graph$arg_df
  plateDF = graph$plate_index_df
  plateNodeDF = graph$plate_node_df

  ##create clusterNameDF to map nodes to plates
  clusterNameDF = plateNodeDF %>%
    dplyr::left_join(plateDF, by = "indexID") %>%
    select(id = nodeID, cluster = indexDisplayName)


  ###make the top line equal to auto_descr (for now ... going with standard output) ... there will always be at least one node
  nodeDF$descLine = ifelse(is.na(nodeDF$auto_descr),
                           nodeDF$auto_label,
                           nodeDF$auto_descr) %>%
                    abbreviate(minlength = wrapWidth)

  ###make the equation line nicely formatted
  ###equation may not be specified
  ###first collapse arg_df, then use in equation line
  eqLineDF = argDF %>%
    filter(argType == "param" |
             (!is.na(argValue) & argType == "arg")) %>%
    mutate(textToCollapse = ifelse(is.na(argValue), argName, argValue)) %>%
    mutate(textToCollapse = ifelse(
      argType == "arg",
      paste0(argName, "=", argValue),
      textToCollapse
    )) %>%
    group_by(rhsID) %>%
    summarize(argList = paste0(textToCollapse, collapse = ","))

  ## create the equation line
  ## if rhs represents a formula, use "label = formula"
  ## if rhs represents distribution, use "label ~ distribution(args)
  ## if rhs is blank, use "label"
  eqDF = nodeDF %>%
    select(id, auto_label, rhs, rhsID, distr) %>%
    left_join(eqLineDF, by = "rhsID") %>%
    mutate(eqLine = ifelse(!distr & !is.na(rhs), paste0(auto_label," = ",rhs),
                           ifelse(is.na(rhs), auto_label,
                                  paste0(auto_label, " ~ ", rhs, "(", argList, ")")))) %>%
    select(id, eqLine)

  ### create nodeDF as DiagrammeR data frame
  nodeDF = nodeDF %>%
    left_join(eqDF, by = "id") %>%
    mutate(type = ifelse(obs == TRUE,"obs","latent"),
           peripheries = ifelse(distr == TRUE | is.na(rhs),1,2),
           fillcolor = ifelse(obs == TRUE,"cadetblue","aliceblue"),
           label = paste0(descLine,"\n",eqLine)) %>%
    select(id,label,type,peripheries,fillcolor) %>%
    left_join(clusterNameDF, by = "id")

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
    edgeDF = DiagrammeR::create_edge_df(from = edgeDF$from,
                                        to = edgeDF$to)
    dgr_graph =  dgr_graph %>% DiagrammeR::add_edge_df(edgeDF)
  }

  ###update global graph attributes
  dgr_graph = dgr_graph %>% causact::setDirectedGraphTheme()

  return(dgr_graph)
}
