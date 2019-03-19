#' @import dplyr
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
