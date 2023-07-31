#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom reticulate import
#' @export
## usethis namespace: end

.onLoad <- function(libname, pkgname) {
  ## preload numpyro and arviz for speedier first calls to numpyro
  reticulate::import("numpyro", delay_load = TRUE)
  reticulate::import("arviz", delay_load = TRUE)
}


