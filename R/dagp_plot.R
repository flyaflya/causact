#' Plot posterior distribution of latent parameters in \code{causact_graph} model.
#'
#' The graph object should be of class \code{causact_graph} and created using \code{dag_create()}.
#' @param drawsDF the dataframe output of \code{dag_greta(mcmc=TRUE)} where each column is a parameter and each row a single draw from a representative sample.
#' @param densityPlot If \code{TRUE}, each parameter gets its own density plot.  If \code{FALSE} (recommended usage), parameters are grouped into facets based on whether they share the same prior or not.  10 and 90 percent credible intervals are displayed for the posterior distributions.
#' @return a credible interval plot of all latent posterior distribution parameters.
#' @examples
#' # A simple example
#' posteriorDF = data.frame(x = rnorm(100),
#' y = rexp(100),
#' z = runif(100))
#' posteriorDF %>%
#' dagp_plot(densityPlot = TRUE)
#'
#' # More complicated example requiring 'greta'
#' \dontrun{
#' library(greta)
#' # Create a 2 node graph
#' graph = dag_create() %>%
#'   dag_node("Get Card","y",
#'          rhs = bernoulli(theta),
#'          data = carModelDF$getCard) %>%
#'   dag_node(descr = "Card Probability by Car",label = "theta",
#'            rhs = beta(2,2),
#'            child = "y")
#' graph %>% dag_render()
#'
#' # below requires Tensorflow installation
#' drawsDF = graph %>% dag_greta(mcmc=TRUE)
#' drawsDF %>% dagp_plot()
#' }
#'
#' # A multiple plate example
#' library(dplyr)
#' poolTimeGymDF = gymDF %>%
#' mutate(stretchType = ifelse(yogaStretch == 1,
#'                             "Yoga Stretch",
#'                             "Traditional")) %>%
#' group_by(gymID,stretchType,yogaStretch) %>%
#'   summarize(nTrialCustomers = sum(nTrialCustomers),
#'             nSigned = sum(nSigned))
#' graph = dag_create() %>%
#'   dag_node("Cust Signed","k",
#'            rhs = binomial(n,p),
#'            data = poolTimeGymDF$nSigned) %>%
#'   dag_node("Probability of Signing","p",
#'            rhs = beta(2,2),
#'            child = "k") %>%
#'   dag_node("Trial Size","n",
#'            data = poolTimeGymDF$nTrialCustomers,
#'            child = "k") %>%
#'   dag_plate("Yoga Stretch","x",
#'             nodeLabels = c("p"),
#'             data = poolTimeGymDF$stretchType,
#'             addDataNode = TRUE) %>%
#'   dag_plate("Observation","i",
#'             nodeLabels = c("x","k","n")) %>%
#'   dag_plate("Gym","j",
#'             nodeLabels = "p",
#'             data = poolTimeGymDF$gymID,
#'             addDataNode = TRUE)
#' graph %>% dag_render()
#' \dontrun{
#' # below requires Tensorflow installation
#' drawsDF = graph %>% dag_greta(mcmc=TRUE)
#' drawsDF %>% dagp_plot()
#' }
#' @importFrom dplyr bind_rows filter group_by
#' @importFrom rlang is_empty UQ enexpr enquo expr_text quo_name eval_tidy .data
#' @export
#' @importFrom ggplot2 ggplot geom_density facet_wrap aes theme_minimal theme scale_alpha_continuous guides labs geom_segment element_blank
#' @importFrom tidyr gather
#' @importFrom cowplot plot_grid
#' @importFrom stats quantile
#' @export

dagp_plot = function(drawsDF,densityPlot = FALSE) { # case where untidy posterior draws are provided
  ..scaled.. <- q95 <- reasonableIntervalWidth <- credIQR <- shape <- param <- NULL ## place holder to pass devtools::check
  if (densityPlot == TRUE) {
    plot = drawsDF %>% gather() %>%
      ggplot(aes(x = value, y = ..scaled..)) +
      geom_density(aes(fill = key)) +
      facet_wrap( ~ key, scales = "free_x") +
      theme_minimal() +
      theme(legend.position = "none")

    plot
  } else { # case where tidy posterior draws are provided
    plotList = list()
    ## filter out NA's like from LKJ prior (we do not know how to plot this)
    tryCatch({
      drawsDF = drawsDF %>%
      addPriorGroups() %>%
        mutate(priorGroup = ifelse(is.na(priorGroup),999999,priorGroup)) %>%
      dplyr::filter(!is.na(priorGroup)) ##if try works, erase this line
    priorGroups = unique(drawsDF$priorGroup)
    numPriorGroups = length(priorGroups)
    for (i in 1:numPriorGroups) {
      df = drawsDF %>% filter(priorGroup == priorGroups[i])

      # create one plot per group
      # groups defined as params with same prior
      plotList[[i]] = df %>% group_by(param) %>%
        summarize(q05 = stats::quantile(value,0.05),
                  q25 = stats::quantile(value,0.55),
                  q45 = stats::quantile(value,0.45),
                  q50 = stats::quantile(value,0.50),
                  q55 = stats::quantile(value,0.55),
                  q75 = stats::quantile(value,0.75),
                  q95 = stats::quantile(value,0.95)) %>%
        mutate(credIQR = q75 - q25) %>%
        mutate(reasonableIntervalWidth = 1.5 * stats::quantile(credIQR,0.75)) %>%
        mutate(alphaLevel = ifelse(.data$credIQR > .data$reasonableIntervalWidth, 0.3,1)) %>%
        arrange(alphaLevel,.data$q50) %>%
        mutate(param = factor(param, levels = param)) %>%
        ggplot(aes(y = param, yend = param)) +
        geom_segment(aes(x = q05, xend = q95, alpha = alphaLevel), size = 4, color = "#5f9ea0") +
        geom_segment(aes(x = q45, xend = q55, alpha = alphaLevel), size = 4, color = "#11114e") +
        scale_alpha_continuous(range = c(0.6,1))  +
        guides(alpha = FALSE) +
        theme_minimal(12) +
        labs(y = element_blank(),
             x = "parameter value",
             caption = ifelse(i == numPriorGroups,"Credible Intervals - 10% (dark) & 90% (light)",""))

    }

    nCol <- ifelse(numPriorGroups==1,1,floor(1 + sqrt(numPriorGroups)))
    cowplot::plot_grid(plotlist = plotList, ncol = nCol)
    },
    error = function(c) dagp_plot(drawsDF, densityPlot = T)) # end try
  } # end else
} # end function
