#' Generate a representative sample of the posterior distribution
#' @description
#' Generate a representative sample of the posterior distribution.  The input graph object should be of class `causact_graph` and created using `dag_create()`.  The specification of a completely consistent joint distribution is left to the user.
#'
#' @param graph a graph object of class `causact_graph` representing a complete and conistent specification of a joint distribution.
#' @param mcmc a logical value indicating whether to sample from the posterior distribution.  When `mcmc=FALSE`, the numpyro code is printed to the console, but not executed.  The user can cut and paste the code to another script for running line-by-line.  This option is most useful for debugging purposes. When `mcmc=TRUE`, the code is executed and outputs a dataframe of posterior draws.
#' @param num_warmup an integer value for the number of initial steps that will be discarded while the markov chain finds its way into the typical set.
#' @param num_samples an integer value for the number of samples.
#' @param seed an integer-valued random seed that serves as a starting point for a random number generator. By setting the seed to a specific value, you can ensure the reproducibility and consistency of your results.
#' @return If `mcmc=TRUE`, returns a dataframe of posterior distribution samples corresponding to the input `causact_graph`.  Each column is a parameter and each row a draw from the posterior sample output.  If `mcmc=FALSE`, running `dag_numpyro` returns a character string of code that would help the user generate the posterior distribution; useful for debugging.
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
#' numpyroCode = graph %>% dag_numpyro(mcmc=FALSE)
#' \dontrun{
#' ## default functionality returns a data frame
#' # below requires numpyro installation
#' drawsDF = graph %>% dag_numpyro()
#' drawsDF %>% dagp_plot()
#' }
#' @importFrom dplyr bind_rows tibble left_join rowwise select add_row as_tibble group_indices row_number mutate filter
#' @importFrom DiagrammeR create_graph add_global_graph_attrs
#' @importFrom rlang enquo expr_text .data expr is_na eval_tidy parse_expr warn
#' @importFrom igraph graph_from_data_frame topo_sort
#' @importFrom tidyr gather
#' @importFrom stats na.omit
#' @import reticulate
#' @export

dag_numpyro <- function(graph,
                      mcmc = TRUE,
                      num_warmup = 1000,
                      num_samples = 4000,
                      seed = 111) {

  ## make sure reticulate autoconfigure is disabled when running this function - I do not think this is needed
  # ac_flag <- Sys.getenv("RETICULATE_AUTOCONFIGURE")
  # on.exit(
  #   Sys.setenv(
  #     RETICULATE_AUTOCONFIGURE = ac_flag
  #   )
  # )
  # Sys.setenv(RETICULATE_AUTOCONFIGURE = FALSE)

  ## initialize to pass devtools check
  newPyName <- dataPy <- id <- auto_data <- dimID <- dec <- NULL ## place holder to pass devtools::check

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
      errorMessage <- paste0("Cannot run dag_numpyro() on given object as it is not a Causact Graph.")
    }
    stop(errorMessage)
  }
  ## Now check the single class
  if(class_g != "causact_graph"){
    errorMessage <- paste0("Cannot run dag_numpyro() on given object as it is not a Causact Graph.")
    stop(errorMessage)
  }

  ## clear cache environment for storing mcmc results
  ## also verify numpyro is available
  if (mcmc) {
    rmExpr = rlang::expr(rm(list = ls()))
    eval(rmExpr, envir = cacheEnv)
    options("reticulate.engine.environment" = cacheEnv)
    pyPacks <- reticulate::py_list_packages()
    packs_to_check <- c("numpyro", "arviz", "xarray")
    existVector = sapply(packs_to_check,
                         function(element) {
                           any(element %in%
                                 pyPacks$package)
                         })
    if (!(all(existVector))){
      rlang::warn("It is likely you need to restart R for dag_numpyro() to make causact's required connection to Python; numpyro or other dependencies are missing from the currently connected Python.  Please restart R, then load the causact package with library(causact).")
    }
    } ## clear cacheEnv

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
  nameChangeStatements = NULL
  importStatements = NULL
  dataStatements = NULL
  plateDataStatements = NULL
  dimStatements = NULL
  functionArguments = NULL
  coordLabelsStatements = NULL
  codeStatements = NULL
  modelStatement = NULL
  posteriorStatement = NULL

  ###IMPORT:   Create code for import statements
  importStatements = "import numpy as np
import numpyro as npo
import numpyro.distributions as dist
import pandas as pd
import arviz as az
from jax import random
from numpyro.infer import MCMC, NUTS
from jax.numpy import transpose as t
from jax.numpy import (exp, log, log1p, expm1, abs, mean,
                 sqrt, sign, round, concatenate, atleast_1d,
                 cos, sin, tan, cosh, sinh, tanh,
                 sum, prod, min, max, cumsum, cumprod )
## note that above is from JAX numpy package, not numpy.\n"

  ###DATA:  Create Code for Data Lines (Nodes that are not in plates)
  ### replace any references to R-Objects with `.` in it to
  ### a renamedObj.  Use new column called dataPy to store name
  ### also handle spaces and colons
  ##punctuation other than underscore
  pattern <- "[^[:^punct:]_$]"
  pattern2 <- "[[:space:]]"  #whitespace
  nodeDF = nodeDF %>%
    mutate(newPyName = grepl(pattern, data, perl = TRUE) |
             grepl(pattern2, data, perl = TRUE)) %>%
    mutate(dataPy = ifelse(newPyName,
                           paste0("renameNodeForPy__",row_number()),
                           data))
  ## create new objects for access from Python if needed
  renameDF = nodeDF %>% filter(newPyName) %>% select(data,dataPy)
  if (NROW(renameDF) > 0) {
    # Loop through each row and copy objects using assign()
    for (i in 1:nrow(renameDF)) {
      old_name <- renameDF$data[i]
      new_name <- renameDF$dataPy[i]
      assign(new_name,
             eval(rlang::parse_expr(old_name)),
             cacheEnv)
      nameChangeStatements = paste0(nameChangeStatements,
                                    new_name, " = ",
                                    old_name,"\n")
    }
  }  ## end create new objects to overcome periods and hyphens

  lhsNodesDF = nodeDF %>%
    dplyr::filter(obs == TRUE | !is.na(data)) %>%
    dplyr::filter(!(label %in% plateDF$indexLabel)) %>%
    dplyr::mutate(codeLine = paste0(auto_label,
                             " = ",
                             "np.array(",
                             paste0("r.",
                                    gsub("\\$", ".",dataPy),
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
    plateDimDF = plateDF %>% dplyr::filter(!is.na(dataNode)) %>%
      mutate(newPyName = grepl(pattern, dataNode, perl = TRUE) |
               grepl(pattern2, dataNode, perl = TRUE)) %>%
      mutate(dataPy = ifelse(newPyName,
                             paste0("renameDimForPy__",row_number()),
                             dataNode))
    if (nrow(plateDimDF) > 0) {
      plateDataStatements = paste(paste0(
        abbrevLabelPad(paste0(plateDimDF$indexLabel)),# four spaces to have invis _dim
                              " = ",
                              "pd.factorize(r.",
        gsub("\\$", ".", plateDimDF$dataPy),
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
      gsub("\\$", ".", plateDimDF$dataPy),
      ",use_na_sentinel=True)[1]   #DIM"),
sep = "\n")
      functionArguments = paste(c(functionArguments,
                                plateDimDF$indexLabel),
                                collapse = ",")

    }

    ## create new objects for access from Python if needed
    renameDIMDF = plateDimDF %>% filter(newPyName) %>% select(dataNode,dataPy)
    if (NROW(renameDIMDF) > 0) {
      # Loop through each row and copy objects using assign()
      for (i in 1:nrow(renameDIMDF)) {
        old_name <- renameDIMDF$dataNode[i]
        new_name <- renameDIMDF$dataPy[i]
        ## this works, but might need to clean up env
        assign(new_name,eval(rlang::parse_expr(old_name)),cacheEnv)
        nameChangeStatements = paste0(nameChangeStatements,
                                      new_name, " = ",
                                      old_name, "\n")
      }
    }  ## end create new objects to overcome periods and hyphens

    ### DEFINE NUMPYRO FUNCTION
    functionName = paste0(graphName,"_model")
    numPyFunStartStatement = paste0(
      paste0("def ",functionName,"("),
      paste(functionArguments,sep = ","),
      "):")

    ### alter nodeDF to have numpyro code
    ### first, add unobserved distribution node code
    modelCodeDF =     nodeDF %>%
    ## get rid of data only nodes - not part of likelihood
    filter(!(obs == TRUE & distr == FALSE)) %>%
    ## narrow down columns to useful ones
    select(id, rhs, obs, rhsID, distr, auto_label, auto_data, dimID, auto_rhs, dec, det, nodeOrder) %>%
    ## add in plate dimension labels
    left_join(dimDF %>% filter(dimType == "plate") %>%
                select(dimID, dimLabel), by = "dimID") %>%
    rowwise() %>%
    ## create code lines for unobserved RV's
    mutate(codeLine = NA) %>% ##init column
    mutate(codeLine =
             ifelse(distr==TRUE & obs == FALSE,
                    paste0(
                      auto_label,
                           " = npo.sample('",
                      auto_label, "', ",
                      rlang::eval_tidy(
                        rlang::parse_expr(auto_rhs)),
                      ")"),
                    codeLine)) %>%
    ## create code lines for observed RV's - likelihoods
    mutate(codeLine =
             ifelse(distr==TRUE & obs == TRUE,
                    paste0(
                      auto_label,
                      " = npo.sample('",
                      auto_label, "', ",
                      rlang::eval_tidy(
                        rlang::parse_expr(auto_rhs)),
                      ",obs=",auto_label,")"),
                    codeLine)) %>%
    ## create code lines for deterministic RV's -- operations
    mutate(codeLine =
             ifelse((distr==FALSE & obs==FALSE),
                    paste0(
                      auto_label,
                      " = npo.deterministic('",
                      auto_label, "', ",
                      ## replace R power(^) with python power(**) and matirx mult(%*%) with (@)
                      gsub("%\\*%", "@",
                           gsub("\\^", "**", auto_rhs)),
                      ")"),
                    codeLine)) %>%
      select(dimLabel,codeLine,auto_label)

    ## create code to handle concatenation in Python
    modelCodeDF = modelCodeDF %>%
      mutate(codeLine = replace_c(codeLine))

  ###Create MODEL function BODY using codeLines from above
    # Using a for loop to iterate over rows
    prevDimLabel = NA
    modelStatement = "\t## Define random variables and their relationships"
    for (i in 1:nrow(modelCodeDF)) {
      currDimLabel = modelCodeDF$dimLabel[i]
      ## node not on plate
      if (rlang::is_na(currDimLabel)) {
        numTabs = 1
        ## for not null add to existing statements
        if (!(rlang::is_null(modelStatement))) {
          modelStatement =
            paste(modelStatement,paste0(paste(rep("\t",
                                          numTabs),
                                     collapse = ""),
                                  modelCodeDF$codeLine[i]),
                sep = "\n")
          } else { ## initialize modelStatement
            modelStatement =
              paste0(paste(rep("\t",
                               numTabs),
                           collapse = ""),
                     modelCodeDF$codeLine[i])
          }
      }
      ## additional line for starting plate
      if (!rlang::is_na(currDimLabel) & !identical(currDimLabel,prevDimLabel)) {
        numTabs = 1
        newLine = paste0("with npo.plate('",
                         currDimLabel,"_dim",
                         "',",currDimLabel,
                         "_dim):")
        modelStatement = paste(modelStatement,
                               paste0(paste(rep("\t",
                                        numTabs),
                                   collapse = ""),
                                    newLine),
              sep = "\n")
      }
      ## node on plate
      if (!rlang::is_na(currDimLabel)) {
        numTabs = 2
        modelStatement =
          paste(modelStatement,paste0(paste(rep("\t",
                                          numTabs),
                                      collapse = ""),
                                     modelCodeDF$codeLine[i]),
                sep = "\n")
      }
      prevDimLabel = currDimLabel
    }

  # get all non-observed / non-formula nodes by default
  # that are not discrete distributions
  discDists = c("bernoulli","binomial","beta_binomial",
                "negative_binomial","hypergeometric",
                "poisson","multinomial","categorical")
  unobservedNodes = graphWithDim$nodes_df %>%
    dplyr::filter(obs == FALSE & distr == TRUE) %>%
    dplyr::filter(!(rhs %in% discDists))%>%
    dplyr::pull(auto_label)



  #group unobserved nodes by their rhs for later plotting by ggplot
  #all nodes sharing the same prior will be graphed on the same scale
  # this code should be moved out of dag_numpyro at some point
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

  ###Create POSTERIOR draws statement
  if (mcmc == TRUE) {  ##clear cacheEnv make sure priorGrp is restored
    ## ensure expected numpyro environment is available
    # Check if the environment is set up
    if (!getOption("causact_env_setup", default = FALSE)) {
      message("In order to use dag_numpyro() for computational Bayesian inference, you must configure a conda Python environment called 'r-causact'.")
      message("To do this, run install_causact_deps().")
      return(invisible())
    }
    assign("priorGroupDF", priorGroupDF, envir = cacheEnv)
    meaningfulLabels(graphWithDim)  ###assign meaningful labels in cacheEnv
  } # end if mcmc=TRUE

  posteriorStatement = paste0("\n# computationally get posterior\nmcmc = MCMC(NUTS(",functionName,"), num_warmup = ",num_warmup,", num_samples = ",num_samples,")")

  rngStatement = paste0("rng_key = random.PRNGKey(seed = ",
                        seed,")")
  if (!rlang::is_null(functionArguments)) {
    runStatement = paste0("mcmc.run(rng_key,",functionArguments,")")
  } else {
    runStatement = paste0("mcmc.run(rng_key)")
  }

  ## format posterior dataframe
  drawsStatement = "drawsDS = az.from_numpyro(mcmc"
  dimToKeepStatement = "dimensions_to_keep = ['chain','draw'"
  unstackToDFStatement = "# unstack plate variables to flatten dataframe as needed\n"
  ## check if any dimensions (i.e. plates)
  if (NROW(plateDimDF > 0)) {
    drawsStatement = paste0(drawsStatement,",\n\tcoords = {'")
    dLabels = unique(stats::na.omit(modelCodeDF$dimLabel))
    numDlabels = length(dLabels)

    for (i in 1:numDlabels) {
      dimToKeepStatement = paste0(dimToKeepStatement,
                                  ",'",
                                  dLabels[i],
                                  "_dim'")
      drawsStatement = paste0(drawsStatement,
                              dLabels[i],
                              "_dim': ",
                              dLabels[i],
                              "_crd")
      unstackToDFStatement = paste0(unstackToDFStatement,
          "for plateLabel in drawsDS['",
          dLabels[i],"_dim']:")

      ## build unstack statement right here
      plateNodes = modelCodeDF %>%
        filter(dimLabel == dLabels[i])
      for (j in 1:NROW(plateNodes)) {
        unstackToDFStatement =
          paste0(unstackToDFStatement,
                 "\n\tnew_varname = f'",
                 plateNodes$auto_label[j],
                 "_{plateLabel.values}'",
                 "\n\tdrawsDS = drawsDS.assign(**{new_varname: drawsDS['",
                 plateNodes$auto_label[j],
                 "'].sel(",
                 dLabels[i],
                 "_dim = plateLabel)})")
      }
      # drop dimension of plate so unstacked DF can be transferred to R
      unstackToDFStatement = paste0(unstackToDFStatement,
                                    "\ndrawsDS = drawsDS.drop_dims('",
                                    dLabels[i],
                                    "_dim')\n")

      ## finish dimToKeep and draws statements below

      if (i == numDlabels) {  ## closing bracket
        drawsStatement = paste0(drawsStatement,"},\n\tdims = {'")
        dimToKeepStatement = paste0(dimToKeepStatement,
                                    "]\n")
      } else {
        ## add comma to keep going
        drawsStatement = paste0(drawsStatement,",\n\t\t'")
      }
    }
    ## now loop over variables to add dimensionality
    plateNodes = modelCodeDF %>% filter(!rlang::is_na(dimLabel))
    numNodes = NROW(plateNodes)
    for (i in 1:numNodes) {
      drawsStatement = paste0(drawsStatement,
                              plateNodes$auto_label[i],
                              "': ['",
                              plateNodes$dimLabel[i],
                              "_dim']")
      if (i == numNodes) {  ## closing bracket
        drawsStatement = paste0(drawsStatement,
                                "}\n\t")
      } else {
        ## add comma to keep going
        drawsStatement = paste0(drawsStatement,",\n\t\t'")
        }
      }
  } else { ## add closing bracket if no plates
    dimToKeepStatement = paste0(dimToKeepStatement,
                                "]\n")
  }

  ## close the statement
  drawsStatement = paste0(drawsStatement,
                            ").posterior\n# prepare xarray dataset for export to R dataframe")
  drawsDFStatement =
    "drawsDS = drawsDS.squeeze(drop = True ).drop_dims([dim for dim in drawsDS.dims if dim not in dimensions_to_keep])\n"
  drawsDFStatement = paste0(dimToKeepStatement,
                            drawsDFStatement,
                            unstackToDFStatement,
                            "\ndrawsDF = drawsDS.squeeze().to_dataframe()")

  ##########################################
  ###Aggregate all code
  codeStatements = c(importStatements,
                     dataStatements,
                     plateDataStatements,
                     dimStatements,
                     coordLabelsStatements,
                     numPyFunStartStatement,
                     modelStatement,
                     posteriorStatement,
                     rngStatement,
                     runStatement,
                     drawsStatement,
                     drawsDFStatement)

  #codeStatements
  ## wrap python code in R to get posterior draws
  codeRun = paste0(nameChangeStatements,
                   'reticulate::py_run_string("\n',
                   paste(codeStatements, collapse = '\n'),
                   '"\n) ## END PYTHON STRING\n',
                   "drawsDF = py$drawsDF")

  ###print out Code as text for user to use
  if(mcmc == FALSE){
    codeForUser = paste0("\n## The below code will return a posterior distribution \n## for the given DAG. Use dag_numpyro(mcmc=TRUE) to return a\n## data frame of the posterior distribution: \n",codeRun)
    message(codeForUser)
  }

  ##EVALUATE CODE IN cacheEnv ENVIRONMENT
  ##make expression out of Code Statements
  ###BELOW LINE COMMENTED DURING DAG_NUMPYRO TESTING
  codeExpr = parse(text = codeRun)

  ##eval expression - use original graph without DIM
  if(mcmc == TRUE) {

    eval(codeExpr, envir = cacheEnv) ## evaluate in other env
    ###return data frame of posterior draws
    return(dplyr::as_tibble(py$drawsDF, .name_repair = "universal"))
  }

  return(invisible(codeForUser))  ## just print code
}

