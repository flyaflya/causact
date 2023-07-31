#' Generate a representative sample of the posterior distribution
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Generate a representative sample of the posterior distribution.  The input graph object should be of class `causact_graph` and created using `dag_create()`.  The specification of a completely consistent joint distribution is left to the user.  Helpful error messages are scheduled for future versions of the `causact` package.
#'
#' @param graph a graph object of class `causact_graph` representing a complete and conistent specification of a joint distribution.
#' @param mcmc a logical value indicating whether to sample from the posterior distribution.  When `mcmc=FALSE`, the numpyro code is printed to the console, but not executed.  The user can cut and paste the code to another script for running line-by-line.  This option is most useful for debugging purposes. When `mcmc=TRUE`, the code is executed and outputs a dataframe of posterior draws.
#' @param ... additional arguments to be passed onto `numpyro`.
#' @return If `mcmc=TRUE`, returns a dataframe of posterior distribution samples corresponding to the input `causact_graph`.  Each column is a parameter and each row a draw from the posterior sample output.  If `mcmc=FALSE`, running `dag_numpyro` returns a character string of code that would help the user generate the posterior distribution
#'
#' @examples
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
#' @importFrom dplyr bind_rows tibble left_join select add_row as_tibble group_indices row_number
#' @importFrom DiagrammeR create_graph add_global_graph_attrs
#' @importFrom rlang enquo expr_text .data expr
#' @importFrom igraph graph_from_data_frame topo_sort
#' @importFrom tidyr gather
#' @importFrom greta mcmc model as_data
#' @export

dag_numpyro <- function(graph,
                      mcmc = TRUE,
                      ...) {

  ## get graph object name for label statement
  graphName = rlang::as_name(rlang::ensym(graph))
  if (graphName == ".") {graphName = get_name(graph)}

  . <- NULL ## place holder to pass devtools::check

  ## First validate that the first argument is indeed a causact_graph
  class_g <- class(graph)
  ## Any causact_graph will have class length of 1
  if(length(class_g) > 1){
  ## This specific case is hard-coded as it has occurred often in early use by the author
    if(class_g[1] == chr("grViz") && class_g[2]=="htmlwidget"){
      errorMessage <- paste0("Given rendered Causact Graph. Check the declaration for a dag_render() call.")
    }
    else {
      errorMessage <- paste0("Cannot run dag_greta() on given object as it is not a Causact Graph.")
    }
    stop(errorMessage)
  }
  ## Now check the single class
  if(class_g != "causact_graph"){
    errorMessage <- paste0("Cannot run dag_greta() on given object as it is not a Causact Graph.")
    stop(errorMessage)
  }

  ###get dimension information
  graphWithDim = graph %>% dag_dim()

  ###update rhs information for labelling computer code
  graphWithDim = rhsPriorComposition(graphWithDim)
  graphWithDim = rhsOperationComposition(graphWithDim)

  ###retrieve nodeDF,edgeDF,argDF,plateIndexDF, and plateNodeDF
  nodeDF = graphWithDim$nodes_df
  edgeDF = graphWithDim$edges_df
  argDF = graphWithDim$arg_df
  plateDF = graphWithDim$plate_index_df
  plateNodeDF = graphWithDim$plate_node_df
  dimDF = graphWithDim$dim_df

  ###arrangeNodes in topological order -> top-down
  nodeIDOrder = igraph::graph_from_data_frame(edgeDF %>% dplyr::select(from,to)) %>%
    igraph::topo_sort(mode = "out") %>%
    names() %>%
    as.integer()

  ## append non-connected nodes into nodeIDOrder
  nodeIDOrder = union(nodeIDOrder,nodeDF$id)

  ## arrange nodeDF by nodeIDOrder
  nodeDF = nodeDF[match(nodeIDOrder,nodeDF$id) , ] %>%
    dplyr::mutate(nodeOrder = dplyr::row_number())

  ###Use DAPROPLIMOPO(DAta,PRior,OPeration,LIkelihood,MOdel,POsterior)
  ###Find all nodes that require data based on user input
  ###Err on the side of including a node

  ### Initialize all the code statements so that NULL
  ### values are skipped without Error
  importStatements = NULL
  dataStatements = NULL
  plateDataStatements = NULL
  dimStatements = NULL
  functionArguments = NULL
  coordLabelsStatements = NULL
  priorStatements = NULL
  opStatements = NULL
  likeStatements = NULL
  # use priorOpLikeDF to use topological order
  # for prior, operation, and likelihood statements
  priorOpLikeDF = data.frame(statement = as.character(NA),
                             orderID = as.integer(NA),
                             stringsAsFactors = FALSE)[-1,]
  priorOpLikeStatements = NULL
  modelStatement = NULL
  posteriorStatement = NULL

  ###IMPORT:   Create code for import statements
  importStatements = "import numpy as np
import numpyro
import numpyro.distributions as dist
import arviz as az
from jax import random
from numpyro.infer import MCMC, NUTS
from jax.scipy.special import expit as invLogit"



  ###DATA:  Create Code for Data Lines (Nodes that are not in plates)
  lhsNodesDF = nodeDF %>%
    dplyr::filter(obs == TRUE | !is.na(data)) %>%
    dplyr::filter(!(label %in% plateDF$indexLabel)) %>%
    dplyr::mutate(codeLine = paste0(auto_label,
                             " = ",
                             "np.array(",
                             paste0("r.",data,
                             ")"))) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #DATA"))

  ###Aggregate Code Statements for DATA
  if(nrow(lhsNodesDF) > 0) {
    dataStatements = paste(lhsNodesDF$codeLine,
                           sep = "\n")
    functionArguments = paste(c(functionArguments,
                              lhsNodesDF$auto_label),
                              collapse = ",")
  }

  ###DIM:  Create code for plate dimensions
    plateDimDF = plateDF %>% dplyr::filter(!is.na(dataNode))
    if (nrow(plateDimDF) > 0) {
      plateDataStatements = paste(paste0(
        abbrevLabelPad(paste0(plateDimDF$indexLabel)),# four spaces to have invis _dim
                              " = ",
                              "pd.factorize(r.",
        gsub("\\$", ".", plateDimDF$dataNode),
                              ",use_na_sentinel=True)[0]   #DIM"),
                              sep = "\n")
    ###make labels for dim variables = to label_dim
      dimStatements = paste(
        paste0(abbrevLabelPad(paste0(plateDimDF$indexLabel,"_dim")),
               " = ",
               "len(np.unique(",
               plateDimDF$indexLabel,
               "))   #DIM"),
        sep = "\n"
      )
      coordLabelsStatements = paste(paste0(
      abbrevLabelPad(paste0(plateDimDF$indexLabel,"_crd")),# four spaces to have invis _dim
      " = ",
      "pd.factorize(r.",
      gsub("\\$", ".", plateDimDF$dataNode),
      ",use_na_sentinel=True)[1]   #DIM"),
sep = "\n")
      functionArguments = paste(c(functionArguments,
                                plateDimDF$indexLabel),
                                collapse = ",")

      }

    ### DEFINE NUMPYRO FUNCTION
    numPyFunStartStatement = paste0(
      paste0("def ",graphName,"_model("),
      paste(functionArguments,sep = ","),
      "):")

    ### alter nodeDF to have numpyro code
    ### first, add unobserved distribution node code
    nodeDF %>%
      rowwise() %>%
      mutate(codeLine =
               ifelse(distr==TRUE & obs == FALSE,
                      paste0(auto_label,
                             " = ",
                             rlang::eval_tidy(rlang::parse_expr(auto_rhs))),NA)) %>%
      select(codeLine)


  ### Prior, Operations, and Likelihood Get Sorted by Topological Order

  ###PRIOR:  Create code for prior lines
  ###create dataframe of dataNodes and their data


  #update auto_rhs to use cbind for R indexing if there is a comma in it

  lhsNodesDF = nodeDF %>%
    dplyr::filter(distr == TRUE & obs == FALSE) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(auto_label),
                             " = ",
                             auto_rhs)) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #PRIOR"))

  ###Aggregate Code Statements for PRIOR
  priorStatements = paste(lhsNodesDF$codeLine,
                          sep = "\n")
  priorOpLikeDF = dplyr::bind_rows(priorOpLikeDF,
                                   data.frame(statement = priorStatements,
                                              orderID = lhsNodesDF$nodeOrder,
                                              stringsAsFactors = FALSE))

  ###OPERATION:  Create code for OPERATION lines
  lhsNodesDF = nodeDF %>%
    dplyr::filter(!is.na(rhs) & distr == FALSE) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(auto_label),
                             " = ",
                             auto_rhs)) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #OPERATION"))

  ###Aggregate Code Statements for OPERATION
  opStatements = paste(lhsNodesDF$codeLine,
                       sep = "\n")
  priorOpLikeDF = dplyr::bind_rows(priorOpLikeDF,
                                   data.frame(statement = opStatements,
                                              orderID = lhsNodesDF$nodeOrder,
                                              stringsAsFactors = FALSE))

  ###LIKELIHOOD:  Create code for LIKELIHOOD lines
  lhsNodesDF = nodeDF %>%
    dplyr::filter(obs == TRUE) %>%  ##only observed nodes
    dplyr::inner_join(edgeDF, by = c("id" = "to")) %>% # only nodes with parents
    dplyr::distinct(.data$id,auto_label,auto_rhs,nodeOrder) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(
      paste0("distribution(",
             auto_label,
             ")")
    ),
    " = ",
    auto_rhs)) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #LIKELIHOOD"))

  ###Aggregate Code Statements for LIKELIHOOD
  likeStatements = paste(lhsNodesDF$codeLine,
                         sep = "\n")
  priorOpLikeDF = dplyr::bind_rows(priorOpLikeDF,
                                   data.frame(statement = likeStatements,
                                              orderID = lhsNodesDF$nodeOrder,
                                              stringsAsFactors = FALSE))

  ### Use topological ordering
  if(nrow(priorOpLikeDF) > 0){
    priorOpLikeStatements = priorOpLikeDF %>%
      dplyr::arrange(orderID) %>%
      dplyr::pull(statement)
  }

  ###Create MODEL Statement
  # get all non-observed / non-formula nodes by default
  # that are not discrete distributions
  discDists = c("bernoulli","binomial","beta_binomial",
                "negative_binomial","hypergeometric",
                "poisson","multinomial","categorical",
                "dirichlet_multinomial")
  unobservedNodes = graphWithDim$nodes_df %>%
    dplyr::filter(obs == FALSE & distr == TRUE) %>%
    dplyr::filter(!(rhs %in% discDists))%>%
    dplyr::pull(auto_label)

  #group unobserved nodes by their rhs for later plotting by ggplot
  #all nodes sharing the same prior will be graphed on the same scale
  # this code should be moved out of dag_greta at some point
  priorGroupDF = graphWithDim$nodes_df %>%
    dplyr::filter(obs == FALSE & distr == TRUE)

  ## replaced dependency on dplyr::group_indices
  ## since its functionality changes fro 0.85 to v1.0
  ## code returns unique group id's based on prior dist
  grpIndexDF = priorGroupDF %>%
    dplyr::select(auto_rhs) %>%
    dplyr::distinct() %>%
    dplyr::mutate(priorGroup = dplyr::row_number())

  ## add priorGroup column
  priorGroupDF = priorGroupDF %>%
    dplyr::left_join(grpIndexDF, by = "auto_rhs")

  assign("priorGroupDF", priorGroupDF, envir = cacheEnv)

  modelStatement = paste0("gretaModel  = model(",
                          paste0(unobservedNodes, collapse = ","),
                          ")   #MODEL")

  ###Create POSTERIOR draws statement
  if (mcmc == TRUE) {  ##clear cacheEnv make sure priorGrp is restored
    rmExpr = rlang::expr(rm(list = ls()))
    eval(rmExpr, envir = cacheEnv)  ## clear cacheEnv
    assign("priorGroupDF", priorGroupDF, envir = cacheEnv)
    meaningfulLabels(graphWithDim)  ###assign meaningful labels in cacheEnv
    labelStatement = NULL
  } else {
  labelStatement = paste0("meaningfulLabels(",
                          graphName,
                          ")") ###assign meaningful labels in cacheEnv
  }
  extraArgList = list(...)
  extraArgString = paste0(paste0(names(extraArgList)," = ", as.character(extraArgList)), collapse = ",")
  mcmcArgs = ifelse(extraArgString == " = ","gretaModel",paste("gretaModel",extraArgString, sep = ","))
  posteriorStatement = paste0("draws       = mcmc(",mcmcArgs,")              #POSTERIOR\ndrawsDF     = replaceLabels(draws) %>% as.matrix() %>%
                dplyr::as_tibble()           #POSTERIOR\ntidyDrawsDF = drawsDF %>% addPriorGroups()  #POSTERIOR\n")

  ##########################################
  ###Aggregate all code
  codeStatements = c(importStatements,
                     dataStatements,
                     plateDataStatements,
                     dimStatements,
                     coordLabelsStatements,
                     numPyFunStartStatement,
                     priorOpLikeStatements,
                     modelStatement,
                     labelStatement,
                     posteriorStatement)

  #codeStatements

  ###print out greta Code as text for user to use
  if(mcmc == FALSE){
    codeForUser = paste0("## The below Python code will return a posterior distribution \n## for the given DAG. Use dag_numpyro(mcmc=TRUE) to return a data frame of\n## the posterior distribution: \n",
         paste(codeStatements, collapse = '\n'))
    message(codeForUser)
  }

  ##EVALUATE CODE IN cacheEnv ENVIRONMENT
  ##make expression out of Code Statements
  ###BELOW LINE COMMENTED FOR DAG_NUMPYRO TESTING
  #codeExpr = parse(text = codeStatements)

  ##eval expression - use original graph without DIM
  if(mcmc == TRUE) {
    eval(codeExpr, envir = cacheEnv) ## evaluate in other env
    ###return data frame of posterior draws
    return(cacheEnv$drawsDF)
  }
  return(invisible(codeForUser))  ## just print code
}
