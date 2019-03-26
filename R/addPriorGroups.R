#' @importFrom dplyr select left_join bind_rows arrange
#' @export

### add prior groups
addPriorGroups = function(tidyDraws) {

  ## if prior groups do not exist, then just return the input
  if(!((exists("priorGroupDF", envir = cacheEnv)) & (exists("meaningfulLabels", envir = cacheEnv)))) {
    return(tidyDraws)
  }

  ## priorGroups exists
  priorGroupDF = get("priorGroupDF", envir = cacheEnv) %>%
    dplyr::select(label,priorGroup)

  ## get root node names for plate nodes
  rootLabelsDF = get('meaningfulLabels',envir = cacheEnv) %>%
    dplyr::select(rootLabel,newNames)

  ## add priorGroup to rootLabel
  priorGroupMappingDF = rootLabelsDF %>%
    dplyr::left_join(priorGroupDF, by = c("rootLabel" = "label")) %>%
    dplyr::select(label = newNames, priorGroup) %>%
    dplyr::bind_rows(priorGroupDF)

  tidyDraws = tidyDraws %>%
    dplyr::left_join(priorGroupMappingDF, by = c("key" = "label")) %>%
    dplyr::arrange(priorGroup,key)

  return(tidyDraws)
}

