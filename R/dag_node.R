#' Add a node to an existing graph object
#'
#' The graph object should be of class \code{dgr_graph} and created using \code{dag_create()}.
#' @param graph a graph object of class \code{dgr_graph} created using \code{dag_create()}.
#' @param label a required character label that describes the node.
#' @param description a longer more descriptive character label for the node.
#' @param distr a variable greta array function such as \code{uniform, normal, lognormal, bernoulli,} etc. Function arguments are optional.  Use \code{distr = NA} to suppress distribution assumptions.  Valid values include \code{greta::normal}, \code{normal}, and \code{normal(6,2)}.
#' @param observed a logical value indicating whether the node is observed.
#' @param formulaString a character string representing an operation that should be performed to calculate the node's value from its parents.  When input, \code{distr} is ignored.
#' @param children an optional character vector of existing node labels or node descriptions.  Directed edges from the newly created node will be created.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph and add 2 nodes by using
#' # the `dag_node()` function twice
#' graph <-
#'   dag_create() %>%
#'   dag_node("X") %>%
#'   dag_node("Y", children = "X")
#' dag_render(graph, shortLabel = TRUE)
#'
#' dag_create() %>%
#'   dag_node("Y","Sales Price",
#'            distr = normal, observed = TRUE) %>%
#'   dag_node("X","Square Footage",
#'            observed = TRUE, distr = NA) %>%
#'   dag_node("mean","Exp Sales Price",
#'            children = "Sales Price",
#'            formulaString = "alpha + beta * x") %>%
#'   dag_node("beta", "Price per Sq Foot",
#'            children = "mean") %>%
#'   dag_node("SD", "Std Dev of Sales Price",
#'            distr = lognormal(0,3),
#'            children = "Sales Price") %>%
#'   dag_node("alpha", "Intercept",
#'            children = "mean")  %>%
#'   dag_edge("Square Footage","mean") %>%
#'   dag_render()
#' @importFrom dplyr select bind_cols as_tibble case_when
#' @importFrom rlang is_empty UQ enexpr enquo expr_text
#' @importFrom DiagrammeR add_node node_data node_aes
#' @importFrom greta uniform normal lognormal bernoulli binomial beta_binomial negative_binomial hypergeometric poisson gamma inverse_gamma weibull exponential pareto student laplace beta cauchy chi_squared logistic f multivariate_normal wishart lkj_correlation multinomial categorical dirichlet dirichlet_multinomial
#' @export
dag_node <- function(graph,
                     descr = as.character(NA),
                     label = as.character(NA),
                     data = NULL, # vector or df
                     rhs = NA, ##not vectorized
                     child = as.character(NA), ##not vectorized
                     obs = FALSE,
                     keepAsDF = FALSE) {

  # handle blank entry
  numArgs = length(match.call())-1
  if(numArgs == 0) {graph = dag_create()
                    descr = "Type ?dag_create"
                    label = "to START MODELLING"}
  if(numArgs == 1) {descr = "Type ?dag_node"
                    label = "to START MODELLING"}

  # capture data as quosure and rhs as expression
  dataQuo = rlang::enquo(data) ##capture as quosure to get env
  dataString = ifelse(is.null(data),NA,rlang::quo_name(dataQuo))
  rhsExpr = rlang::enexpr(rhs) ##distribution or formula
  rhsList = rhsDecomp(!!rhsExpr)  ## get distr flag,
        #fcn name (i.e. formula string for formula),
        #paramDF (distr-TRUE only),argDF (distr or formula)

  # create dataframe for RHS arguments
  rhsID = max(graph$arg_df$rhsID,0) + 1

  argDF = dplyr::bind_rows(list(param=rhsList$paramDF, arg=rhsList$argDF), .id = 'argType')
  if(nrow(argDF) > 0) { ##add rhsID as first column
    argDF = cbind(rhsID=rhsID,argDF)
  } else { ##add rhsID as column with zero rows
      argDF$rhsID[-1] = as.integer(NA)
  }


  # rhsString is either formula, dist function, or not given
  if(rhsList$fcn == "NA") { # decomp returns string, not NA
    rhsString = NA
    } else {
      rhsString = rhsList$fcn
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
      data = dataString,
      rhs = rhsString,
      #distr or formula
      child = child,
      obs = obs,
      rhsID = rhsID,
      distr = rhsList$distr,
      stringsAsFactors = FALSE)


  ### complete graph object
  graph$nodes_df = dplyr::bind_rows(graph$nodes_df,ndf)
  graph$arg_df = dplyr::bind_rows(graph$arg_df,argDF)

  ## add edges for newly added nodes with non-na children
  edgeDF = ndf %>% dplyr::filter(!is.na(child))
  if(nrow(edgeDF) > 0) {
    fromVector = edgeDF$id
    toVector = edgeDF$child
    graph = graph %>% dag_edge(fromVector,toVector)
  }

  ### update labels for plotting
  graph = autoLabel(graph)

  return(graph)
}
