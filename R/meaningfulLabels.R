#' Store meaningful parameter labels
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Store meaningful parameter labels as as part of running `dag_numpyro()`.  When `numpyro` creates posterior distributions for multi-dimensional parameters, it creates an often meaningless number system for the parameter (e.g. beta\[1,1\], beta\[2,1\], etc.).  Since parameter dimensionality is often determined by a `factor`, this function creates labels from the factors unqiue values.  `replaceLabels()` applies the text labels stored using this function to the `numpyro` output.  The meaningful parameter names are stored in an environment, `cacheEnv`.
#' @param graph a `causact_graph` object.
#' @return a data frame `meaningfulLabels` stored in an environment named `cacheEnv` that contains a lookup table between greta labels and meaningful labels.
#'
#' @importFrom dplyr filter left_join select pull as_tibble mutate_all bind_rows
#' @import rlang
#' @importFrom purrr map
#' @importFrom tidyr unite
#' @export

meaningfulLabels = function(graph) {

  ###get dimension information if not there
  if (nrow(graph$dim_df) > 0) {
    graphWithDimensions = graph}
  else {
    graphWithDimensions = graph %>% dag_dim()
  }

  ###update rhs information for labelling computer code
  graphWithDimensions = rhsPriorComposition(graphWithDimensions)
  graphWithDimensions = rhsOperationComposition(graphWithDimensions)

  ## function makes an environment called cacheEnv where
  ## it stores meaningful labels for all the plate parameters
  ###retrieve nodeDF,edgeDF,argDF,plateIndexDF, and plateNodeDF
  nodeDF = graphWithDimensions$nodes_df
  edgeDF = graphWithDimensions$edges_df
  argDF = graphWithDimensions$arg_df
  plateDF = graphWithDimensions$plate_index_df
  plateNodeDF = graphWithDimensions$plate_node_df
  dimDF = graphWithDimensions$dim_df
  ## if plateDataStaement is NULL, just return empty DF
  if(nrow(plateDF %>% dplyr::filter(!is.na(dataNode))) == 0) {
    relabelDF = data.frame(oldNames = as.character(NA),
                           newNames = as.character(NA),
                           rootLabel = as.character(NA),
                           stringsAsFactors = FALSE)[-1,]
    assign("meaningfulLabels", relabelDF, envir = cacheEnv)
    return(invisible())
  }


  ### within posterior change generic coef labels to names
  ### future: consider not doing this by default
  renamingMultiDimParamsDF = plateDF %>% # get plates
    dplyr::filter(!is.na(dataNode)) %>% #only applies to plates with data
    dplyr::left_join(plateNodeDF, by = "indexID") %>% ## find nodes on plates with data
    dplyr::select(indexLabel, nodeID,dataNode) %>% #connect labels and nodes
    dplyr::left_join(nodeDF, by = c("nodeID" = "id")) %>% # get node info
    dplyr::select(nodeID, indexLabel, label, rhsID, dataNode) %>%
    dplyr::left_join(dimDF %>% dplyr::filter(dimType == "plate"), by = c("nodeID" = "nodeID", "indexLabel" = "dimLabel")) %>%
    dplyr::select(label, indexLabel, dimValue, dataNode)

  ## compress indexLabel and dimValue labels

  ## create relabelDF
  relabelDF = NULL

  if (nrow(renamingMultiDimParamsDF) > 0) {
    uniqueLabels = unique(renamingMultiDimParamsDF$label)
    for (i in seq_along(uniqueLabels)) {
      tempDF = renamingMultiDimParamsDF %>%
        dplyr::filter(label == uniqueLabels[i])
      dimensionList = purrr::map(tempDF$dimValue,.f = function(x) { return(1:x)})
      oldNamesDF = do.call(expand.grid,dimensionList) %>%
        tidyr::unite(label, sep = ",") %>%
        dplyr::mutate(bigLabel = paste0(uniqueLabels[i],"[",label,"]"))
      oldNames = oldNamesDF %>%
        dplyr::pull(bigLabel)
        ## add [*,,1] back in if only one dim value given to match greta output
      oldNames = ifelse(stringr::str_detect(oldNames,","),
             oldNames,
             stringr::str_replace(oldNames,"]",",1]"))

      namesDF = tempDF %>%
        dplyr::as_tibble() %>%
        dplyr::select(indexLabel,dataNode) %>%
        dplyr::mutate(levelNames = purrr::map(.x = dataNode,.f = getLevelNames))

      ## the default sort order given by getLevelNames is alphabetic
      ## with str_sort used and numeric = TRUE argument
      ## below assumes this sort is used
      newNamesDF = do.call(expand.grid,namesDF$levelNames) %>%
        dplyr::mutate_all(function(x){abbreviate(x, minlength = 8)}) %>%
        tidyr::unite(label, sep = "_") %>%
        dplyr::mutate(bigLabel = paste0(uniqueLabels[i],"_",label))

      newNames = newNamesDF %>% dplyr::pull(bigLabel)

        names(newNames) = oldNames
        relabelDF = dplyr::bind_rows(relabelDF,
                                     data.frame(oldNames = oldNames,
                                                newNames = newNames,
                                                rootLabel = uniqueLabels[i],
                                                stringsAsFactors = FALSE))
    }# end for
  } else {# end if
    relabelDF = data.frame(oldNames = as.character(NA),
                           newNames = as.character(NA),
                           rootLabel = as.character(NA),
                           stringsAsFactors = FALSE)[-1,]
} #end else
  ##

  ## assign cached values to a variable
  assign("meaningfulLabels", relabelDF, envir = cacheEnv)

  return(invisible())

}  ###assigns named vector - value of meaningful labels in cacheEnv
