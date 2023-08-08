#' Group together latent parameters by prior distribution.
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Add a column to a tidy dataframe of draws that groups parameters by their prior distribution.  All parameters with the same prior distribution receive the same index.
#' @param drawsDF the dataframe created by  `dag_numpyro()` where each row represents one draw of MCMC output.  Two columns are expected, `param` - the parameter name, `value` - the realized value, and a third column, `priorGroup`, is appended as an integer grouping parameters by their prior distributions. The data for this third column is stored in an environment called `cacheEnv` when the `dag_numpyro()` function is called.  Any parameters with the same prior end up in the same prior group; used by `dagp_plot()` to group parameters when plotted.
#' @return a tidy dataframe of posterior draws. Useful for passing to `dagp_plot()` or for creating plots using `ggplot()`.
#' @importFrom dplyr select left_join bind_rows arrange distinct
#' @importFrom lifecycle badge
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
    dplyr::select(rootLabel,newNames) %>%
    dplyr::distinct()

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

