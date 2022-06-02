#' Replace parameter labels in a `mcmc.list` with more meaningful labels
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Replace parameter labels in a `mcmc.list` with more meaningful labels after they are created by running `dag_greta()`.  When `greta` creates posterior distributions for multi-dimensional parameters, it creates an often meaningless number system for the parameter (e.g. beta[1,1], beta[2,1], etc.).  Since parameter dimensionality is often determined by a `factor`, this functionality restores the text labels associated with the underlying factor whose coefficients are being estimated (e.g. beta_varValue1, beta_varValue2).  The meaningful parameter names are stored in an environment, `cacheEnv`, created by a call to `dag_greta()`.
#' @param draws an `mcmc.list` object created by  `dag_greta()`.
#' @return an `mcmc.list` with more meanignful names that get created during a `dag_greta` function call.
#' `r lifecycle::badge('experimental')`
#' @importFrom dplyr mutate left_join pull
#' @importFrom coda varnames
#' @export


replaceLabels = function(draws) {
  existingColNames = coda::varnames(draws)
  nameLookupDF = get('meaningfulLabels',envir = cacheEnv)
  if(nrow(nameLookupDF) > 0) {
  newNames = data.frame(oldNames = existingColNames,
                        stringsAsFactors = FALSE) %>%
    dplyr::left_join(nameLookupDF, by = "oldNames") %>%
    dplyr::mutate(newNames = ifelse(is.na(newNames),oldNames,newNames)) %>%
    dplyr::pull(newNames)
  coda::varnames(draws) <- newNames
  }
  return(draws)
}
