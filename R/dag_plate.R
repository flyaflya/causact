#' Create a plate representation for repeated nodes.
#'
#' Given a graph object of class \code{causact_graph}, create collections of nodes that should be repeated i.e. represent multiple instances of a random variable, random vector, or random matrix.  When nodes are on more than one plate, graph rendering will treat each unique combination of plates as separate plates.
#' @param graph a graph object of class \code{dgr_graph} created using \code{dag_create()}.
#' @param descr a longer more descriptive label for the cluster/plate.
#' @param label a short character string to use as an index.
#' @param nodeLabels a character vector of node labels or descriptions to include in the list of nodes.
#' @param data a vector representing the categorical data whose unique values become the plate index.  To use with \code{addDataNode = TRUE}, this vector should represent observations of a variable that can be coerced to a factor.
#' @param addDataNode a logical value.  When \code{addDataNode = TRUE}, the code attempts to add a node of observed data that is used as an index for extracting the correct parameter from parent nodes that are on the newly created plate.  Verify the graphical model using \code{dag_render()} to ensure correct behavior.
#' @param rhs Optional \code{rhs} expression for when \code{addDataNode = TRUE}.  This can be either a greta distribution such as \code{uniform, normal, lognormal, bernoulli,} etc. or an R expression. Greta distribution arguments are optional.  Valid values include \code{normal(mu,sigma)},\code{greta::normal}, \code{normal}, and \code{normal(6,2)}.  R computation/expression examples include \code{alpha+beta*x} or \code{ilogit(alpha + gamma + beta)}.  If a distribution is given, this is a random/stochastic node, if a formula is given it is a deterministic node once given the values of its parents.  Quotes should not be used as all function/computations should consist of R objects, functions, and constants.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # create small DF
#' dag_create() %>%
#'     dag_node("House Prices","Y") %>%
#'     dag_plate("Observation","i",nodeLabels = "Y") %>%
#'     dag_render(shortLabel = TRUE)
#'
#' # bigger example
#' graph = dag_create() %>%
#' dag_node("Get Card","y",
#'          rhs = bernoulli(theta),
#'          data = carModelDF$getCard) %>%
#'   dag_node(descr = "Card Probability by Car",label = "theta",
#'            rhs = beta(2,2),
#'            child = "y") %>%
#'   dag_node("Car Model","x",
#'            data = carModelDF$carModel,
#'            child = "y") %>%
#'   dag_plate("Car Model","x",
#'             data = carModelDF$carModel,
#'             nodeLabels = "theta")
#' #' graph %>% dag_render()
#'
#' # multiple plate example
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
#' @importFrom dplyr mutate
#' @importFrom rlang enquo get_expr UQ
#' @importFrom utils head
#' @export

dag_plate <- function(graph,
                      descr,
                      label,
                      nodeLabels,
                      data = as.character(NA),
                      addDataNode = FALSE,
                      rhs = NA ) {

  ## Validate that the first argument is indeed a causact_graph
  class_g <- class(graph)
  ## Any causact_graph will have class length of 1
  if(length(class_g) > 1){
    ## This specific case is hard-coded as it has occured often in early use by the author
    if(class_g[1] == chr("grViz") && class_g[2]=="htmlwidget"){
      errorMessage <- paste0("Given rendered Causact Graph. Check the declaration for a dag_render() call.")
    }
    else {
      errorMessage <- paste0("Cannot add dag_plate() to given object as it is not a Causact Graph.")
    }
    stop(errorMessage)
  }
  ## Now check the single class
  if(class_g != "causact_graph"){
    errorMessage <- paste0("Cannot add dag_plate() to go given object as it is not a Causact Graph.")
    stop(errorMessage)
  }

  ### capture data argument as string
  dataNodeExpr = rlang::enexpr(data)
  dataNodeString = rlang::expr_text(dataNodeExpr)
  if(is.na(head(data,1)[1])) {dataNodeString = as.character(NA)}

  ## rhs used for adding distribution to observed node
  ## for plates, the observed node is added automatically
  rhsExpr = rlang::enexpr(rhs) ##distribution or formula

  ### get nodeIDS for enterned node labels
  ### node labels can be labels, descr, or data
  nodeIDS = findNodeID(graph,nodeLabels)

  ## update plate index DF
  lastPlateIndex = max(graph$plate_index_df$indexID, 0)
  graph$plate_index_df = dplyr::add_row(
    graph$plate_index_df,
    indexID = lastPlateIndex + 1,
    indexLabel = label,
    indexDescription = descr,
    indexDisplayName = paste0(descr, " ", label),
    dataNode = dataNodeString
  )

  ## update plate node df
  for (i in seq_along(nodeIDS)) {
    graph$plate_node_df = dplyr::add_row(graph$plate_node_df,
                                          indexID = lastPlateIndex + 1,
                                          nodeID = nodeIDS[i])
  }

  ##

  ## add data node to extract the proper parameter
  if (!is.na(dataNodeString) & addDataNode == TRUE) {
    graph = graph %>% addPlateDataNode(plateIndex = lastPlateIndex + 1,
                                       rhs = !!rhsExpr)
  }

  return(graph)  ## return updated graph
}
