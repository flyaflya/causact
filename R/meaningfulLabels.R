#' @import dplyr rlang
meaningfulLabels = function(graphWithDimensions,plateDataStatements) {

  ## if plateDataStaement is NULL, just return empty DF
  if(is.null(plateDataStatements)) {
    relabelDF = data.frame(oldNames = as.character(NA),
               newNames = as.character(NA),
               stringsAsFactors = FALSE)[-1,]
    assign("meaningfulLabels", relabelDF, envir = cacheEnv)
    return(invisible())
  }

  ###retrieve nodeDF,edgeDF,argDF,plateIndexDF, and plateNodeDF
  nodeDF = graphWithDimensions$nodes_df
  edgeDF = graphWithDimensions$edges_df
  argDF = graphWithDimensions$arg_df
  plateDF = graphWithDimensions$plate_index_df
  plateNodeDF = graphWithDimensions$plate_node_df
  dimDF = graphWithDimensions$dim_df

  ###within posterior change generic coef labels to names
  ### future: consider not doing this by default
  renamingMultiDimParamsDF = plateDF %>%
    dplyr::filter(!is.na(dataNode)) %>%
    left_join(plateNodeDF, by = "indexID") %>%
    select(indexLabel, nodeID) %>%
    left_join(nodeDF, by = c("nodeID" = "id")) %>%
    select(nodeID, indexLabel, label, rhsID) %>%
    left_join(dimDF %>% dplyr::filter(dimType == "plate"), by = "nodeID") %>%
    select(label, indexLabel, dimValue)

  ## create relabelDF
  relabelDF = NULL

  if (nrow(renamingMultiDimParamsDF) > 0) {
    for (i in 1:nrow(renamingMultiDimParamsDF)) {
      ### only handle one dimensional plates at this point
      if (length(renamingMultiDimParamsDF$dimValue[i] == 1)) {
        oldNames = paste0(
          renamingMultiDimParamsDF$label[i],
          "[",
          1:renamingMultiDimParamsDF$dimValue[i],
          ",1]"
        )
        ## reproduce global DIM variables in function environment
        ## separateLHS and rhs  (lhs gets assigned in local environment based on rhs executed in global environment
        lhs = stringr::word(plateDataStatements)
        rhs = stringr::str_remove(plateDataStatements,lhs) %>%
          stringr::str_remove(" <- ")
        assign(lhs,rlang::eval_tidy(rlang::parse_expr(rhs), env = rlang::global_env()), env = rlang::current_env())
        newNames = rlang::eval_tidy(rlang::parse_expr(
          paste0("levels(",
                 renamingMultiDimParamsDF$indexLabel[i],
                 ")")
        ),
        env = rlang::current_env())
        newParamVectorValues = paste0(
          renamingMultiDimParamsDF$label[i],
          "[",
          abbreviate(newNames, minlength = 8),
          "]"
        )
        names(newParamVectorValues) = oldNames
        relabelDF = data.frame(oldNames = oldNames, newNames = newParamVectorValues, stringsAsFactors = FALSE)
      } #end if
    }# end for
  }# end if

  ##

  ## assign cached values to a variable
  assign("meaningfulLabels", relabelDF, envir = cacheEnv)
  return(invisible())

}  ###assigns named vector - value of meaningful labels in cacheEnv
