#' Generate a representative sample of the posterior distribution
#' @description
#' `r lifecycle::badge('defunct')`
#'
#' This function is currently defunct.  It has been superseded by `dag_numpyro()` because of tricky and sometimes unresolvable installation issues related to the greta package's use of tensorflow.  If the greta package resolves those issues, this function may return, but please use `dag_numpyro()` as a direct replacement.
#'
#' Generate a representative sample of the posterior distribution.  The input graph object should be of class `causact_graph` and created using `dag_create()`.  The specification of a completely consistent joint distribution is left to the user.  Helpful error messages are scheduled for future versions of the `causact` package.
#'
#' @param graph a graph object of class `causact_graph` representing a complete and conistent specification of a joint distribution.
#' @param mcmc a logical value indicating whether to sample from the posterior distribution.  When `mcmc=FALSE`, the greta code is printed to the console, but not executed.  The user can cut and paste the code to another script for running line-by-line.  This option is most useful for debugging purposes. When `mcmc=TRUE`, the code is executed and outputs a dataframe of posterior draws.
#' @param meaningfulLabels a logical value indicating whether to replace the indexed variable names in `draws` with abbreviated names representing the factor value corresponding to the index.  This argument is treated as `TRUE` regardless of user input.  The ability to retain numerical indexing will be in a subsequent release.
#' @param ... additional arguments to be passed onto `greta::mcmc()`.
#' @return If `mcmc=TRUE`, returns a dataframe of posterior distribution samples corresponding to the input `causact_graph`.  Each column is a parameter and each row a draw from the posterior sample output.  If `mcmc=FALSE`, running `dag_greta` returns a character string of code that would help the user create three objects representing the posterior distribution:
#' \enumerate{
#' \item `draws`:  An mcmc.list object containing raw output from the HMCMC sampler used by `greta`.
#' \item `drawsDF`:  A wide data frame with all latent variables as columns and all draws as rows.  This data frame is useful for calculations based on the posterior
#' \item `tidyDrawsDF`:  A long data frame with each draw represented on one line.  This data frame is useful for plotting posterior distributions.
#' }
#'
#' @examples
#' library(greta)
#' graph = dag_create() %>%
#'   dag_node("Get Card","y",
#'            rhs = bernoulli(theta),
#'            data = carModelDF$getCard) %>%
#'   dag_node(descr = "Card Probability by Car",label = "theta",
#'            rhs = beta(2,2),
#'            child = "y") %>%
#'   dag_node("Car Model","x",
#'            data = carModelDF$carModel,
#'            child = "y") %>%
#'   dag_plate("Car Model","x",
#'             data = carModelDF$carModel,
#'             nodeLabels = "theta")
#'
#' graph %>% dag_render()
#' gretaCode = graph %>% dag_greta(mcmc=FALSE)
#' \dontrun{
#' ## default functionality returns a data frame
#' # below requires Tensorflow installation
#' drawsDF = graph %>% dag_greta()
#' drawsDF %>% dagp_plot()
#' }
#' @export

dag_greta <- function(graph,
                      mcmc = TRUE,
                      meaningfulLabels = TRUE,
                      ...) {
  message("This function is currently defunct.  It has been superseded by dag_numpyro() because of tricky installation issues related to the greta package's use of tensorflow.  If the greta package resolves those issues, this function may return, but please use dag_numpyro() as a direct replacement.")

}
