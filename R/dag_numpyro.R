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
#' @importFrom dplyr bind_rows tibble left_join rowwise select add_row as_tibble group_indices row_number mutate filter join_by
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
                        seed = 1234567) {

  ## initialize to pass devtools check
  newPyName <- dataPy <- id <- auto_data <- dimID <- dec <- plateStmnt <- numTabsForNode <- plateLabelling <- varLabelling <- selLabelling <- forLoop <- newVar <- dimNum <- plateState <- plateLabelState <- varNameStmnt <- id <- selStmnt <- NULL

  ## get graph object name for label statement
  graphName = rlang::as_name(rlang::ensym(graph))
  if (graphName == ".") {graphName = get_name(graph)}

  . <- NULL

  ## Validate input class
  class_g <- class(graph)
  if(length(class_g) > 1){
    if(class_g[1] == chr("grViz") && class_g[2]=="htmlwidget"){
      stop("Given rendered Causact Graph. Check the declaration for a dag_render() call.")
    } else {
      stop("Cannot run dag_numpyro() on given object as it is not a Causact Graph.")
    }
  }
  if(class_g != "causact_graph"){
    stop("Cannot run dag_numpyro() on given object as it is not a Causact Graph.")
  }

  ## clear cache env; verify numpyro
  if (mcmc) {
    rmExpr = rlang::expr(rm(list = ls()))
    eval(rmExpr, envir = cacheEnv)
    options("reticulate.engine.environment" = cacheEnv)
    pyPacks <- reticulate::py_list_packages()
    if (!any("numpyro" %in% pyPacks$package)){
      rlang::warn("It is likely you need to restart R for dag_numpyro() to make causact's required connection to Python; numpyro or other dependencies are missing from the currently connected Python.  Please restart R, then load the causact package with library(causact).")
    }
  }

  ## Compose graph
  graphWithDim = graph %>% dag_dim()
  graphWithDim = rhsPriorComposition(graphWithDim)
  graphWithDim = rhsOperationComposition(graphWithDim)

  ## data frames
  nodeDF = graphWithDim$nodes_df
  edgeDF = graphWithDim$edges_df
  argDF = graphWithDim$arg_df
  plateDF = graphWithDim$plate_index_df
  plateNodeDF = graphWithDim$plate_node_df
  dimDF = graphWithDim$dim_df

  ## topo order
  nodeIDOrder = igraph::graph_from_data_frame(edgeDF %>% dplyr::select(from,to)) %>%
    igraph::topo_sort(mode = "out") %>% names() %>% as.integer()
  nodeIDOrder = union(nodeIDOrder,nodeDF$id)
  nodeDF = nodeDF[match(nodeIDOrder,nodeDF$id) , ] %>%
    dplyr::mutate(nodeOrder = dplyr::row_number())

  ## Code statement holders
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

  ## IMPORTS
  importStatements = "import numpy as np
import numpyro as npo
import numpyro.distributions as dist
import pandas as pd
from jax import random
from numpyro.infer import MCMC, NUTS
from jax.numpy import transpose as t
from jax.numpy import (exp, log, log1p, expm1, abs, mean,
                 sqrt, sign, round, concatenate, atleast_1d,
                 cos, sin, tan, cosh, sinh, tanh,
                 sum, prod, min, max, cumsum, cumprod )
## note that above is from JAX numpy package, not numpy.\n"

  ## DATA nodes (non-plate)
  pattern <- "[^[:^punct:]_$]"
  pattern2 <- "[[:space:]]"
  nodeDF = nodeDF %>%
    mutate(newPyName = grepl(pattern, data, perl = TRUE) |
             grepl(pattern2, data, perl = TRUE)) %>%
    mutate(dataPy = ifelse(newPyName,
                           paste0("renameNodeForPy__",row_number()),
                           data))
  renameDF = nodeDF %>% filter(newPyName) %>% select(data,dataPy)
  if (NROW(renameDF) > 0) {
    for (i in 1:nrow(renameDF)) {
      old_name <- renameDF$data[i]
      new_name <- renameDF$dataPy[i]
      assign(new_name, eval(rlang::parse_expr(old_name)), cacheEnv)
      nameChangeStatements = paste0(nameChangeStatements, new_name, " = ", old_name, "\n")
    }
  }

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

  if(nrow(lhsNodesDF) > 0) {
    dataStatements = paste(lhsNodesDF$codeLine, sep = "\n")
    functionArguments = paste(c(functionArguments, lhsNodesDF$auto_label), collapse = ",")
  }

  ## PLATE dimensions
  plateDimDF = plateDF %>% dplyr::filter(!is.na(dataNode)) %>%
    mutate(newPyName = grepl(pattern, dataNode, perl = TRUE) |
             grepl(pattern2, dataNode, perl = TRUE)) %>%
    mutate(dataPy = ifelse(newPyName,
                           paste0("renameDimForPy__",row_number()),
                           dataNode))
  if (nrow(plateDimDF) > 0) {
    plateDataStatements = paste(paste0(
      abbrevLabelPad(paste0(plateDimDF$indexLabel)),
      " = ",
      "pd.factorize(np.array(r.",
      gsub("\\$", ".", plateDimDF$dataPy),
      "),use_na_sentinel=True)[0]   #DIM"),
      sep = "\n")
    dimStatements = paste(
      paste0(abbrevLabelPad(paste0(plateDimDF$indexLabel,"_dim")),
             " = ",
             "len(np.unique(",
             plateDimDF$indexLabel,
             "))   #DIM"),
      sep = "\n"
    )
    coordLabelsStatements = paste(paste0(
      abbrevLabelPad(paste0(plateDimDF$indexLabel,"_crd")),
      " = ",
      "pd.factorize(np.array(r.",
      gsub("\\$", ".", plateDimDF$dataPy),
      "),use_na_sentinel=True)[1]   #DIM"),
      sep = "\n")
    functionArguments = paste(c(functionArguments, plateDimDF$indexLabel), collapse = ",")
  }

  renameDIMDF = plateDimDF %>% filter(newPyName) %>% select(dataNode,dataPy)
  if (NROW(renameDIMDF) > 0) {
    for (i in 1:nrow(renameDIMDF)) {
      old_name <- renameDIMDF$dataNode[i]
      new_name <- renameDIMDF$dataPy[i]
      assign(new_name, eval(rlang::parse_expr(old_name)), cacheEnv)
      nameChangeStatements = paste0(nameChangeStatements, new_name, " = ", old_name, "\n")
    }
  }

  ## MODEL function header
  functionName = paste0(graphName,"_model")
  numPyFunStartStatement = paste0(paste0("def ",functionName,"("),
                                  paste(functionArguments,sep = ","),
                                  "):")

  ## MODEL body
  modelCodeDF = nodeDF %>%
    filter(!(obs == TRUE & distr == FALSE)) %>%
    select(id, rhs, obs, rhsID, distr, auto_label, auto_data, dimID, auto_rhs, dec, det, nodeOrder) %>%
    left_join(getPlateStatements(graphWithDim), by = join_by(id == nodeID, auto_label == auto_label)) %>%
    rowwise() %>%
    mutate(codeLine = NA) %>%
    mutate(codeLine =
             ifelse(distr==TRUE & obs == FALSE,
                    paste0(
                      auto_label,
                      " = npo.sample('",
                      auto_label, "', ",
                      rlang::eval_tidy(rlang::parse_expr(auto_rhs)),
                      ")\n"),
                    codeLine)) %>%
    mutate(codeLine =
             ifelse(distr==TRUE & obs == TRUE,
                    paste0(
                      auto_label,
                      " = npo.sample('",
                      auto_label, "', ",
                      rlang::eval_tidy(rlang::parse_expr(auto_rhs)),
                      ",obs=",auto_label,")\n"),
                    codeLine)) %>%
    mutate(codeLine =
             ifelse((distr==FALSE & obs==FALSE),
                    paste0(
                      auto_label,
                      " = npo.deterministic('",
                      auto_label, "', ",
                      gsub("%\\*%", "@", gsub("\\^", "**", auto_rhs)),
                      ")\n"),
                    codeLine)) %>%
    select(dimLabel = indexLabel,codeLine,auto_label,plateStmnt,numTabsForNode,plateLabelling,varLabelling,selLabelling,det)

  modelCodeDF = modelCodeDF %>%
    mutate(codeLine = replace_c(codeLine)) %>%
    mutate(numTabsForNode = ifelse(rlang::is_na(numTabsForNode),1,numTabsForNode))

  prevDimLabel = NA
  modelStatement = "\t## Define random variables and their relationships"
  for (i in 1:nrow(modelCodeDF)) {
    currDimLabel = modelCodeDF$dimLabel[i]
    numTabs = modelCodeDF$numTabsForNode[i]
    if (rlang::is_na(currDimLabel)) {
      if (!(rlang::is_null(modelStatement))) {
        modelStatement =
          paste(modelStatement,
                paste0(paste(rep("\t", numTabs), collapse = ""),
                       modelCodeDF$codeLine[i]),
                sep = "\n")
      } else {
        modelStatement =
          paste0(paste(rep("\t", numTabs), collapse = ""),
                 modelCodeDF$codeLine[i])
      }
    }
    if (!rlang::is_na(currDimLabel) & !identical(currDimLabel,prevDimLabel)) {
      newLine = modelCodeDF$plateStmnt[i]
      modelStatement = paste0(modelStatement,"\n", newLine)
    }
    if (!rlang::is_na(currDimLabel)) {
      modelStatement =
        paste0(modelStatement,
               paste(rep("\t",numTabs),collapse = ""),
               modelCodeDF$codeLine[i])
    }
    prevDimLabel = currDimLabel
  }

  ## Names created via npo.deterministic → drop later
  detNames <- nodeDF %>%
    dplyr::filter(distr == FALSE & obs == FALSE) %>%
    dplyr::pull(auto_label) %>% unique()
  if (length(detNames) > 0) {
    dropNamesStatement <- paste0(
      "drop_names = {", paste0("'", detNames, "'", collapse = ", "), "}"
    )
  } else {
    dropNamesStatement <- "drop_names = set()"
  }

  ## misc
  discDists = c("bernoulli","binomial","beta_binomial",
                "negative_binomial","hypergeometric",
                "poisson","multinomial","categorical")
  priorGroupDF = graphWithDim$nodes_df %>%
    dplyr::filter(obs == FALSE & distr == TRUE)
  grpIndexDF = priorGroupDF %>%
    dplyr::select(auto_rhs) %>%
    dplyr::distinct() %>%
    dplyr::mutate(priorGroup = dplyr::row_number())
  priorGroupDF = priorGroupDF %>% dplyr::left_join(grpIndexDF, by = "auto_rhs")

  if (mcmc == TRUE) {
    if (!getOption("causact_env_setup", default = FALSE)) {
      message("In order to use dag_numpyro() for computational Bayesian inference, you must configure a conda Python environment called 'r-causact'.")
      message("To do this, run install_causact_deps().")
      return(invisible())
    }
    assign("priorGroupDF", priorGroupDF, envir = cacheEnv)
    meaningfulLabels(graphWithDim)
  }

  posteriorStatement = paste0("\n# computationally get posterior\nmcmc = MCMC(NUTS(",functionName,"), num_warmup = ",num_warmup,", num_samples = ",num_samples,")")
  rngStatement = paste0("rng_key = random.PRNGKey(seed = ", seed,")")
  if (!rlang::is_null(functionArguments)) {
    runStatement = paste0("mcmc.run(rng_key,",functionArguments,")")
  } else {
    runStatement = paste0("mcmc.run(rng_key)")
  }

  ## maps for axis labels and rv->axes
  if (nrow(plateDimDF) > 0) {
    axisLabelsStatement <- paste0(
      "axis_labels = {",
      paste(
        paste0("'", plateDimDF$indexLabel, "_dim': ", plateDimDF$indexLabel, "_crd"),
        collapse = ", "
      ),
      "}"
    )
  } else {
    axisLabelsStatement <- "axis_labels = {}"
  }

  rvAxesDF <- dimDF %>%
    dplyr::filter(dimType == "plate") %>%
    dplyr::arrange(nodeID, dimLabel) %>%
    dplyr::group_by(nodeID) %>%
    dplyr::summarise(labels_vec = list(paste0(dimLabel, "_dim")), .groups = "drop") %>%
    dplyr::left_join(nodeDF %>% dplyr::select(id, auto_label),
                     by = dplyr::join_by(nodeID == id))
  if (nrow(rvAxesDF) > 0) {
    entries <- mapply(function(name, axes) {
      paste0("'", name, "': [", paste0("'", axes, "'", collapse = ", "), "]")
    }, rvAxesDF$auto_label, rvAxesDF$labels_vec, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    rvAxesStatement <- paste0("rv_to_axes = {", paste(entries, collapse = ", "), "}")
  } else {
    rvAxesStatement <- "rv_to_axes = {}"
  }

  ## collect samples
  drawsStatement <- "samples = mcmc.get_samples(group_by_chain=True)  # dict: name -> [chains, draws, *axes]"

  ## Build labeled DataFrame; drop deterministic RVs; 1-based fallback indices
  drawsDFStatement <- paste(
    "import numpy as np, pandas as pd",
    "# Flatten (chains*draws) and expand RVs using rv_to_axes + axis_labels",
    "flat = {name: np.reshape(val, (-1,) + val.shape[2:]) for name, val in list(samples.items())}",
    "out = {}",
    "for name, arr in list(flat.items()):",
    "    # Drop deterministic RVs entirely",
    "    if name in drop_names:",
    "        continue",
    "    axes = rv_to_axes.get(name, [])  # [] if not present",
    "    # If scalar per draw, keep as a single column",
    "    if arr.ndim == 1:",
    "        out[name] = arr",
    "        continue",
    "    # Otherwise (vector/matrix/...): expand to separate columns",
    "    trailing = arr.shape[1:]",
    "    arr2 = arr.reshape(arr.shape[0], int(np.prod(trailing)))",
    "    for j in range(arr2.shape[1]):",
    "        idx = np.unravel_index(j, trailing)",
    "        name_parts = []",
    "        for axis, i in enumerate(idx):",
    "            if axis < len(axes):",
    "                axis_name = axes[axis]",
    "                labels = axis_labels.get(axis_name)",
    "                if labels is not None:",
    "                    labs = np.asarray(labels).astype(str)",
    "                    name_parts.append(labs[i] if i < labs.shape[0] else str(i + 1))",  # 1-based fallback
    "                else:",
    "                    name_parts.append(str(i + 1))",  # 1-based fallback when mapped but unlabeled",
    "            else:",
    "                name_parts.append(str(i + 1))",  # 1-based fallback for unmapped extra axis",
    "        col = f'{name}[{','.join(name_parts)}]'",
    "        out[col] = arr2[:, j]",
    "drawsDF = pd.DataFrame(out)",
    sep = "\n"
  )

  ## Aggregate and execute
  codeStatements = c(
    importStatements,
    dataStatements,
    plateDataStatements,
    dimStatements,
    coordLabelsStatements,
    numPyFunStartStatement,
    modelStatement,
    posteriorStatement,
    rngStatement,
    runStatement,
    axisLabelsStatement,
    rvAxesStatement,
    dropNamesStatement,   # NEW: drop deterministic RVs
    drawsStatement,
    drawsDFStatement
  )

  codeRun = paste0(
    nameChangeStatements,
    'reticulate::py_run_string("\n',
    paste(codeStatements, collapse = '\n'),
    '"\n) ## END PYTHON STRING\n',
    "drawsDF = reticulate::py$drawsDF"
  )

  if(mcmc == FALSE){
    codeForUser = paste0("\n## The below code will return a posterior distribution \n## for the given DAG. Use dag_numpyro(mcmc=TRUE) to return a\n## data frame of the posterior distribution: \n",codeRun)
    message(codeForUser)
  }

  codeExpr = parse(text = codeRun)

  if(mcmc == TRUE) {
    eval(codeExpr, envir = cacheEnv)
    return(dplyr::as_tibble(py$drawsDF, .name_repair = "universal"))
  }

  return(invisible(codeForUser))
}
