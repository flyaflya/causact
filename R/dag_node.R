#' Add a node to an existing \code{causact_graph} object
#'
#' The graph object should be of class \code{causact_graph} and created using \code{dag_create()}.
#' @param graph a graph object of class \code{causact_graph}.  An initial object gets created using \code{dag_create()}.
#' @param descr a longer more descriptive character label for the node.
#' @param label a shorter character label for referencing the node (e.g. "X","beta").
#' @param rhs either a greta distribution such as \code{uniform, normal, lognormal, bernoulli,} etc. or an R expression. Greta distribution arguments are optional.  Valid values include \code{normal(mu,sigma)},\code{greta::normal}, \code{normal}, and \code{normal(6,2)}.  R computation/expression examples include \code{alpha+beta*x} or \code{ilogit(alpha + gamma + beta)}.  If a distribution is given, this is a random/stochastic node, if a formula is given it is a deterministic node once given the values of its parents.  Quotes should not be used as all function/computations should consist of R objects, functions, and constants.
#' @param child an optional character vector of existing node labels.  Directed edges from the newly created node to the supplied nodes will be created.
#' @param data a vector or data frame (with observations in rows and variables in columns).
#' @param obs a logical value indicating whether the node is observed.  Assumed to be \code{TRUE} when \code{data} argument is given.
#' @param keepAsDF a logical value indicating whether the \code{data} argument should be split into one random varaible node per column or kept together as a random matrix for matrix computation.  Defaults to creating one node per column of the data frame.
#' @param extract a logical value.  When TRUE, child nodes will try to extract an indexed value from this node.  When FALSE, the entire random object (e.g. scalar, vector, matrix) is passed to children nodes.  Only use this argument when overriding default behavior seen using \code{dag_render()}.
#' @param dec a logical value indicating whether the node is a decision node.  Used to show nodes as rectangles instead of ovals when using \code{dag_render()}.
#' @param det a logical value indicating whether the node is a deterministic function of its parents  Used to draw a double-line (i.e. peripheries = 2) around a shape when using \code{dag_render()}.  Assumed to be \code{TRUE} when \code{rhs} is a formula.
#' @return a graph object of class \code{causact_graph} with an additional node(s).
#' @examples
#' library(greta)
#' # Create an empty graph and add 2 nodes by using
#' # the `dag_node()` function twice
#' graph2 = dag_create() %>%
#'   dag_node("Get Card","y",
#'          rhs = bernoulli(theta),
#'          data = carModelDF$getCard) %>%
#'   dag_node(descr = "Card Probability by Car",label = "theta",
#'            rhs = beta(2,2),
#'            child = "y")
#' graph2 %>% dag_render()
#'
#'
#' # The Eight Schools Example from Gelman et al.:
#'
#' schools_dat <- data.frame(y = c(28,  8, -3,  7, -1,  1, 18, 12),
#' sigma = c(15, 10, 16, 11,  9, 11, 10, 18), schoolName = paste0("School",1:8))
#'
#' graph = dag_create() %>%
#'   dag_node("Treatment Effect","y",
#'            rhs = normal(theta, sigma),
#'            data = schools_dat$y) %>%
#'   dag_node("Std Error of Effect Estimates","sigma",
#'            data = schools_dat$sigma,
#'            child = "y") %>%
#'   dag_node("Exp. Treatment Effect","theta",
#'            child = "y",
#'            rhs = avgEffect + schoolEffect) %>%
#'   dag_node("Pop Treatment Effect","avgEffect",
#'            child = "theta",
#'            rhs = normal(0,30)) %>%
#'   dag_node("School Level Effects","schoolEffect",
#'            rhs = normal(0,30),
#'            child = "theta") %>%
#'   dag_plate("Observation","i",nodeLabels = c("sigma","y","theta")) %>%
#'   dag_plate("School Name","school",
#'             nodeLabels = "schoolEffect",
#'             data = schools_dat$schoolName,
#'             addDataNode = TRUE)
#'
#' graph %>% dag_render()
#' \dontrun{
#' # below requires Tensorflow installation
#' graph %>% dag_greta(mcmc=TRUE)
#' tidyDrawsDF %>% dagp_plot()
#' }
#' @importFrom dplyr bind_rows filter
#' @importFrom rlang is_empty UQ enexpr enquo expr_text quo_name eval_tidy
#' @importFrom greta uniform normal lognormal bernoulli binomial beta_binomial negative_binomial hypergeometric poisson gamma inverse_gamma weibull exponential pareto student laplace beta cauchy chi_squared logistic f multivariate_normal wishart lkj_correlation multinomial categorical dirichlet dirichlet_multinomial %*% apply backsolve chol2inv colMeans cov2cor colSums diag eigen forwardsolve identity rowMeans rowSums sweep tapply
#' @export
dag_node <- function(graph,
                     descr = as.character(NA),
                     label = as.character(NA),
                     rhs = NA, ##not vectorized
                     child = as.character(NA), ##not vectorized
                     data = NULL, # vector or df
                     obs = FALSE,
                     keepAsDF = FALSE,
                     extract = as.logical(NA),
                     dec = FALSE,
                     det = FALSE) {

  # handle blank entry -- user enters zero arguments
  numArgs = length(match.call())-1
  if(numArgs == 0) {graph = dag_create()
                    descr = "Type ?dag_create"
                    label = "to START MODELLING"}
  if(numArgs == 1) {descr = "Type ?dag_node"
                    label = "to START MODELLING"}

  ## Validate that the first argument is indeed a causact_graph
  class_g <- class(graph)
  ## Any causact_graph will have class length of 1
  if(length(class_g) > 1){
    ## This specific case is hard-coded as it has occured often in early use by the author
    if(class_g[1] == chr("grViz") && class_g[2]=="htmlwidget"){
      errorMessage <- paste0("Given rendered Causact Graph.Check the declaration for a dag_render() call.")
    }
    else {
      errorMessage <- paste0("Cannot add dag_node() to given object as it is not a Causact Graph.")
    }
    stop(errorMessage)
  }
  ## Now check the single class
  if(class_g != "causact_graph"){
    errorMessage <- paste0("Cannot add dag_node() to given object as it is not a Causact Graph.")
    stop(errorMessage)
  }

  # capture data as quosure and rhs as expression
  dataQuo = rlang::enquo(data) ##capture as quosure to get env
  dataString = ifelse(is.null(data),NA,rlang::quo_name(dataQuo))
  rhsExpr = rlang::enexpr(rhs) ##distribution or formula
  childString = ifelse(rlang::is_empty(child),as.character(NA),paste(child, collapse = ",")) ## test whether child has values... if so make string to store values in nodeDF

  # capture the parameters and argument of the rhs expression
  # also, update value of distr to signal whether distribution or formula
  if (is.na(rlang::expr_text(rhsExpr)) | rlang::expr_text(rhsExpr) == "NA") {
    rhsString = NA
    rhsDistr = FALSE
    rhsID = NA   ##signals that rhs is blank
  } else {
    rhsList = rhsDecomp(!!rhsExpr)  ## get distr flag,
    #fcn name (i.e. formula string for formula),
    #paramDF (distr-TRUE only),argDF (distr or formula)

    # create dataframe for RHS arguments
    rhsID = max(graph$arg_df$rhsID, 0) + 1

    argDF = dplyr::bind_rows(list(param = rhsList$paramDF, arg = rhsList$argDF), .id = 'argType')
    if (nrow(argDF) > 0) {
      ##add rhsID as first column
      argDF = cbind(rhsID = rhsID, argDF)
    } else {
      ##add rhsID as column with zero rows
      argDF$rhsID[-1] = as.integer(NA)
    }

    rhsString = rhsList$fcn
    rhsDistr = rhsList$distr
  }

  # determine if data is present
  # data makes for an observed node
  if (!is.na(dataString) & keepAsDF == FALSE) {
    numberOfNodes = max(NCOL(rlang::eval_tidy(dataQuo)), length(descr), length(label))
    if (is.data.frame(rlang::eval_tidy(dataQuo))) {
      baseDF = all.vars(dataQuo)[1]
      dataString = paste0(baseDF, "$", colnames(rlang::eval_tidy(dataQuo)))
    }
    obs = TRUE
  } else if (!is.na(dataString) & keepAsDF == TRUE) {
    numberOfNodes = 1
    dataString = dataString
    obs = TRUE
  } else {
    dataString = as.character(NA)  ## restore as NA
    numberOfNodes = max(length(descr), length(label))
  }



  ## initialize nodeDF info for this node(s)
  nodeIDstart = max(graph$nodes_df$id,0) + 1
  ndf <-
    data.frame(
      id = nodeIDstart:(nodeIDstart+numberOfNodes-1),
      # user entered quantities
      label = label,
      descr = descr,
      rhs = rhsString,
      child = childString, ##store string of child names
      data = dataString,
      #distr or formula
      obs = obs,
      rhsID = rhsID,
      distr = rhsDistr,
      dec = dec,
      det = det,
      stringsAsFactors = FALSE)


  ### complete graph object
  graph$nodes_df = dplyr::bind_rows(graph$nodes_df,ndf)
  if(!is.na(rhsID)) {graph$arg_df = dplyr::bind_rows(graph$arg_df,argDF)}

  ## add edges for newly added nodes with non-na children
  edgeDF = ndf %>% dplyr::filter(!is.na(child))
  if(!is.na(child[1]) & length(child) > 0) {
    fromVector = edgeDF$id
    toVector = child  ## use vector of child names not string
    if(is.na(extract)) {
      graph = graph %>% dag_edge(fromVector,toVector)
  } else if(extract == TRUE) {
    graph = graph %>% dag_edge(fromVector,toVector, type = "extract")
  } else {
    graph = graph %>% dag_edge(fromVector,toVector, type = "solid")
  }
  }

  ### update labels for plotting
  graph = autoLabel(graph)

  return(graph)
}
