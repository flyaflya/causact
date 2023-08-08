#' Convert graph to Diagrammer object for visualization
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Convert a `causact_graph` to a `DiagrammeR` object for visualization.
#' @param graph a graph object of class `causact_graph` created using `dag_create()`.
#' @param wrapWidth a required character label that describes the node.
#' @param shortLabel a longer more descriptive character label for the node.
#' @return a graph object of class `dgr_graph`.  Useful for further customizing graph displays using the `DiagrammeR` package.
#' @examples
#' library("DiagrammeR")
#' dag_create() %>%
#' dag_node("Get Card","y",
#'          rhs = bernoulli(theta),
#'          data = carModelDF$getCard) %>%
#'   dag_diagrammer() %>%
#'   render_graph(title = "DiagrammeR Version of causact_graph")
#' @importFrom dplyr select filter group_by summarize mutate left_join
#' @importFrom stringr str_replace
#' @importFrom DiagrammeR create_node_df create_graph add_node_df create_edge_df
#' @importFrom rlang .data
#' @importFrom lifecycle badge
#' @export


dag_diagrammer = function(graph, wrapWidth = 24, shortLabel = FALSE) {

  shape <- NULL ## place holder to pass devtools::check

  ## First validate that the first argument is indeed a causact_graph
  class_g <- class(graph)
  ## Any causact_graph will have class length of 1
  if(length(class_g) > 1){
    ## This specific case is hard-coded as it has occured often in early use by the author
    if(class_g[1] == chr("grViz") && class_g[2]=="htmlwidget"){
      errorMessage <- paste0("Given rendered Causact Graph. Check the declaration for a dag_render() call.")
    }
    else {
      errorMessage <- paste0("Cannot convert given object to a Diagrammer object as it is not a Causact Graph.")
    }
    stop(errorMessage)
  }
  ## Now check the single class
  if(class_g != "causact_graph"){
    errorMessage <- paste0("Cannot convert given object to a Diagrammer object as it is not a Causact Graph.")
    stop(errorMessage)
  }

  # add dimension labels
  graph = graph %>% dag_dim()

  ### line needed because DiagrammeR does not support
  ### nested or intersecting subgraphs
  if(nrow(graph$plate_index_df)>1) {
    graph = graph %>% pseudoPlate()
  }

  ###retrieve nodeDF,edgeDF,argDF,plateIndexDF, and plateNodeDF
  nodeDF = graph$nodes_df
  edgeDF = graph$edges_df
  argDF = graph$arg_df
  plateDF = graph$plate_index_df
  plateNodeDF = graph$plate_node_df
  dimDF = graph$dim_df

  ###make the top line equal to auto_descr (for now ... going with standard output) ... there will always be at least one node
  nodeDF$descLine = ifelse(is.na(nodeDF$auto_descr),
                           nodeDF$auto_label,
                           nodeDF$auto_descr) %>%
                    abbreviate(minlength = wrapWidth)

  ###section for adding dimension to description
  ###only DO this for nodes that are not on plates
  nodesOnPlates = unique(plateNodeDF$nodeID)
  dimLabelDF = dimDF %>%
    dplyr::filter(dimValue > 1 &
                    dimType != "plate" &
                    !(nodeID %in% nodesOnPlates)) %>%
    dplyr::group_by(nodeID) %>%
    dplyr::summarize(dimLabel = paste0(dimValue, collapse = "\U00D7")) %>%
    dplyr::mutate(dimLabel = paste0(" [",dimLabel,"]"))

  ###BELOW CODE ###
  ###IT ADDS DIMENSION AFTER NODE DESCRIPTION###
  ###LIKE CAR MODEL [1000]###
  ###ONLY DO THIS IF NODE NOT ON PLATE###
  nodeDF = nodeDF %>%
    dplyr::left_join(dimLabelDF,by = c("id" = "nodeID")) %>%
    dplyr::mutate(dimLabel = ifelse(is.na(dimLabel),"",dimLabel)) %>%
    dplyr::mutate(descLine = paste0(descLine,dimLabel))

  ###make the equation line nicely formatted
  ###equation may not be specified
  ###first collapse arg_df, then use in equation line
  eqLineDF = argDF %>%
    dplyr::filter(argType == "param" |
             (!is.na(argValue) & argType == "arg")) %>%
    dplyr::mutate(argNameWithDim = ifelse(is.na(argDimLabels),argName,
                                          paste0(argValue,"[",argDimLabels,"]"))) %>%
    dplyr::mutate(textToCollapse = ifelse(is.na(argValue), argName, argValue)) %>%  dplyr::mutate(textToCollapse = ifelse(is.na(argDimLabels), textToCollapse, argNameWithDim)) %>%
    dplyr::mutate(textToCollapse = ifelse(
      argType == "arg",
      paste0(argName, "=", argValue),
      textToCollapse
    )) %>%
    dplyr::group_by(rhsID) %>%
    dplyr::summarize(argList = paste0(textToCollapse, collapse = ","))

  ## create the equation line
  ## substitute dimLabels in formulas .. this section can be cleaned up
  ## but seems to work for now
  labelsToReplaceDF = argDF %>%
    dplyr::filter(!is.na(argDimLabels)) %>%  ## get arguments with added dim
    dplyr::mutate(newArgName = paste0(argName,"[",argDimLabels,"]")) %>%
    dplyr::select(rhsID,oldArgName = argName,newArgName)

  ## for each label to replace, go through nodeDF and replace string
  if (nrow(labelsToReplaceDF) > 0) {
    for (i in 1:nrow(labelsToReplaceDF)) {
      positionIndex = which(nodeDF$rhsID == labelsToReplaceDF$rhsID[i])
      for (j in seq_along(positionIndex)) {
        nodeDF$rhs[positionIndex[j]] = stringr::str_replace(nodeDF$rhs[positionIndex[j]],
                                             labelsToReplaceDF$oldArgName[i],
                                             labelsToReplaceDF$newArgName[i])
      }
    }
  }



  ## if rhs represents a formula, use "label = formula"
  ## if rhs represents distribution, use "label ~ distribution(args)
  ## if rhs is blank, use "label"
  eqDF = nodeDF %>%
    dplyr::select("id", auto_label, rhs, rhsID, distr) %>%
    dplyr::left_join(eqLineDF, by = "rhsID") %>%
    dplyr::mutate(eqLine = ifelse(!distr & !is.na(rhs), paste0(auto_label," = ",rhs),
                           ifelse(is.na(rhs), auto_label,
                                  paste0(auto_label, " ~ ", rhs, "(", argList, ")")))) %>%
    dplyr::select("id", eqLine)

  ### create nodeDF as DiagrammeR data frame
  ##create clusterNameDF to map nodes to plates
  ##if plate has no data, assume it is an observation plate
  ##use bottom observed node dimension to guess dimension of plate
  plateWithNoDataNodesDF = plateDF %>% dplyr::filter(is.na(dataNode)) %>%
    dplyr::left_join(plateNodeDF, by = "indexID") %>%
    dplyr::left_join(dimDF, by = "nodeID") %>%
    dplyr::filter(dimType == "row" & dimValue > 1) %>%
    dplyr::select(indexID, dimValue) %>%
    dplyr::distinct()
  plateDF = plateDF %>%
    dplyr::left_join(plateWithNoDataNodesDF, by = "indexID") %>%
    dplyr::mutate(indexDisplayName = ifelse(is.na(dimValue),
                                            indexDisplayName,
                                            paste0(indexDisplayName," [",dimValue,"]")))
  clusterNameDF = plateNodeDF %>%
    dplyr::left_join(plateDF, by = "indexID") %>%
    dplyr::select(id = nodeID, cluster = indexDisplayName, clusterShortLabel = indexDisplayName)

  nodeDF = nodeDF %>%
    dplyr::left_join(eqDF, by = "id") %>%
    dplyr::mutate(type = ifelse(obs == TRUE,"obs","latent"),
    shape = ifelse(.data$dec == TRUE, "rect", "ellipse"),
    peripheries = ifelse((distr == TRUE | is.na(rhs)) & det == FALSE,1,2),
    fillcolor = ifelse(obs == TRUE,"cadetblue","aliceblue"),
           label = ifelse(descLine == eqLine,descLine,paste0(descLine,"\n",eqLine))) %>%  ###poor man's version of shortLabel
    dplyr::select("id",label,type,"shape",peripheries,fillcolor,auto_descr) %>%
    dplyr::left_join(clusterNameDF, by = "id") %>%
    dplyr::arrange(.data$id)  ## id is not passed to create_node_df(), so this ensures proper order

  ###just use description if shortLabel = TRUE
  if(shortLabel == TRUE) {
    nodeDF$label = sapply(strwrap(nodeDF$auto_descr,
                                  width = wrapWidth,
                                  simplify = FALSE),
                          paste,
                          collapse = "\n")
    nodeDF$cluster = nodeDF$clusterShortLabel
    }


  ### DiagrammeR checks for existence of cluster
  ### column and sends error if all NA
  ### so remove cluster column from DF if all NA
  if (all(is.na(nodeDF$cluster))) {
    ### use DIagrammeR::create_node_df
    nodeDF = DiagrammeR::create_node_df(
      n = nrow(nodeDF),
      label = nodeDF$label,
      type = nodeDF$type,
      shape = nodeDF$shape,
      peripheries = nodeDF$peripheries,
      fillcolor = nodeDF$fillcolor
    )
  } else {
    ### use DIagrammeR::create_node_df
    ### change NA values to "" for DiagrammeR
    nodeDF = DiagrammeR::create_node_df(
      n = nrow(nodeDF),
      label = nodeDF$label,
      type = nodeDF$type,
      shape = nodeDF$shape,
      peripheries = nodeDF$peripheries,
      fillcolor = nodeDF$fillcolor,
      cluster = nodeDF$cluster
    )
  }



  # create new graph
  dgr_graph = DiagrammeR::create_graph(directed = TRUE) %>%
    DiagrammeR::add_node_df(nodeDF)

  ### add egdes if applicable
  if (nrow(edgeDF) > 0) {
    ## use dashed for type = extract
    edgeDF$style = ifelse(edgeDF$type != "extract" | is.na(edgeDF$type),"solid","dashed")
    edgeDF = DiagrammeR::create_edge_df(from = edgeDF$from,
                                        to = edgeDF$to,
                                        style = edgeDF$style)
    dgr_graph =  dgr_graph %>% DiagrammeR::add_edge_df(edgeDF)
  }

  ###update global graph attributes
  dgr_graph = dgr_graph %>% causact::setDirectedGraphTheme()

  return(dgr_graph)
}
