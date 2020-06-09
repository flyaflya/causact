#' Add a column to tidy dataframe of draws that groups parameters by their prior distribution.  All parameters with the same prior distribution recieve the same index.
#' @param drawsDF the dataframe created by  \code{dag_greta()} where each row represents one draw of MCMC output.  Two columns are expected, \code{param} - the parameter name, \code{value} - the realized value, and a third column, \code{priorGroup}, is appended as an integer grouping parameters by their prior distributions. The data for this third column is stored in an environment called \code{cacheEnv} when the \code{dag_greta()} function is called.  Any parameters with the same prior end up in the same prior group.  Used by \code{dagp_plot()} to group parameters when plotted.
#' @return a tidy dataframe of posterior draws. Useful for passing to \code{dagp_plot()} or for creating plots using \code{ggplot()}.
#'
#' @importFrom dplyr select left_join bind_rows arrange
#' @export

### add prior groups
addPriorGroups = function(drawsDF) {

  ## if prior groups do not exist, then just return the input
  if(!((exists("priorGroupDF", envir = cacheEnv)) & (exists("meaningfulLabels", envir = cacheEnv)))) {
    return(drawsDF)
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

  tidyDrawsDF = drawsDF %>% gather() %>%
    dplyr::left_join(priorGroupMappingDF, by = c("key" = "label")) %>%
    dplyr::rename(param = key) %>%
    dplyr::arrange(priorGroup,.data$param)

  return(tidyDrawsDF)
}

