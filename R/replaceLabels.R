#' Replace parameter labels in a \code{mcmc.list} with more meaningful labels after they are created by running \code{dag_greta()}.  When \code{greta} creates posterior distributions for multi-dimensional parameters, it creates an often meaningless number system for the parameter (e.g. beta[1,1], beta[2,1], etc.).  Since parameter dimensionality is often determined by a \code{factor}, this functionality restores the text labels associated with the underlying factor whose coefficients are being estimated (e.g. beta_varValue1, beta_varValue2).  The meaningful parameter names are stored in an environment, \code{cacheEnv}, created by a call to \code{dag_greta()}.
#' @param draws an \code{mcmc.list} object created by  \code{dag_greta()}.
#' @return an \code{mcmc.list} with more meanignful names that get created during a \code{dag_greta} function call.
#'
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
