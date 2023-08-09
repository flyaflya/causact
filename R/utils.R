#' @importFrom forcats fct_relevel
#' @importFrom stringr str_sort str_detect
#' @importFrom rlang ensym as_name get_expr set_expr is_primitive eval_bare get_env node_car is_call
#### assume RHS is a distribution
#### if distribution, isolate distribution name,
#### parameter names/values, and argument names/values
#### if formula, isolate object names
#### goal is to auto-produce parent nodes
rhsDecompDistr = function(rhs) {
  ## read in expression as a quosure
  distExpr = rlang::enexpr(rhs)
  distString = as.character(distExpr)

  ## create data frame of named arguments and their position
  namedArgDF = data.frame(
    position = as.integer(NA),
    argName = as.character(NA),
    stringsAsFactors = FALSE
  )[-1, ]
  if (!is.null(names(distExpr))) {
    ## at least one named arg
    namedArgDF = data.frame(
      position = 1:(length(names(distExpr)) - 1),
      argName = names(distExpr)[2:length(names(distExpr))],
      stringsAsFactors = FALSE
    )
  }

  ## get top function name (note: this assumes namespace is stripped)
  fnName = rlang::call_name(distExpr)

  ## function arguments list
  allArgs = names(formals(fnName)) ##function arguments  ##note this used to use formalArgs, but did not work with beta distribution

  ## fill in missing arguments in list
  for (i in seq_along(allArgs)) {
    if (!(allArgs[i] %in% namedArgDF$argName)) {
      #arg Name missing
      insertIndex = which(namedArgDF$argName == "")[1]
      if (!(is.na(insertIndex))) {
        # blank insert spot available
        namedArgDF$argName[insertIndex] = allArgs[i] ##fill by arg position
      } else {
        # add arg to end of data frame
        namedArgDF = rbind(
          namedArgDF,
          data.frame(
            position = nrow(namedArgDF) + 1,
            argName = allArgs[i],
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }  ## end for loop
  ## add in known values
  numberOfKnownArgumentValues = length(distString) - 1
  namedArgDF$argValue = NA
  if (numberOfKnownArgumentValues > 0) {
    namedArgDF$argValue[1:numberOfKnownArgumentValues] =
      distString[2:length(distString)]
  }

  ## sort based on standard parameter ordering
  allArgsDF = data.frame(argName = allArgs, stringsAsFactors = FALSE) %>%
    dplyr::left_join(namedArgDF, by = "argName") %>%
    dplyr::select(-position)
  rm(namedArgDF)

  ## for all distributions, one of the below words signals
  ## the transition from parameter arguments to other arguments
  numParents = min(which(allArgs %in% c("dim", "dimension", "n_realisations"))) - 1  ## get number of dist paramaters
  if(rlang::is_empty(numParents)){ numParents = nrow(allArgsDF) }  ## some distributions are missing dim argument, so assume (for now) all arguments are parameters

  paramDF = allArgsDF[1:numParents,]
  ## fill in missing arg values with argName
  paramDF$argValue = ifelse(is.na(paramDF$argValue),paramDF$argName,paramDF$argValue)

  ## if there are more arguments than parameter arguments, add them to argDF
  if(nrow(allArgsDF) > numParents) {
    argDF = allArgsDF[(numParents + 1):nrow(allArgsDF),]
  } else {
    argDF = allArgsDF[-1,]  ## create empty dataframe
  }

  ## shorten truncation and dimension argName for convenience
  argDF$argName = sub("truncation", "trunc", argDF$argName)
  argDF$argName = sub("dimension", "dim", argDF$argName)

  z = list(
    distr = TRUE,
    fcn = fnName,
    paramDF = paramDF,
    argDF = argDF
  )
  return(z)
}

###formula for decomposing a formula
###and placing its parent nodes in paramDF
rhsDecompFormula = function(rhs) {
  ## read in expression as a quosure
  formExpr = rlang::enexpr(rhs)
  formString = as.character(formExpr)
  argName = all.vars(formExpr)
  argValue = argName ##populate both with same value for now...might change
  fnName = rlang::expr_deparse(formExpr)
  paramDF = data.frame(argName = argName,
                       argValue = argValue,
                       stringsAsFactors = FALSE)
  argDF = data.frame(
    argName = as.character(NA),
    argValue = as.character(NA),
    stringsAsFactors = FALSE
  )[-1,]

  z = list(
    distr = FALSE,
    fcn = fnName,
    paramDF = paramDF,
    argDF = argDF
  )

  return(z)
}

rhsDecompMixJointDistr = function(rhs) {
  ## read in expression as a quosure
  distExpr = rlang::enexpr(rhs)
  distString = as.character(distExpr)

  ## create data frame of named arguments and their position
  namedArgDF = data.frame(
    position = as.integer(NA),
    argName = as.character(NA),
    argValue = as.character(NA),
    stringsAsFactors = FALSE
  )[-1, ]

  ## get top function name (note: this assumes namespace is stripped)
  fnName = rlang::call_name(distExpr)

  ## get params (i.e. dists passed via ... to mix)
  ## and non-param arguments like dim as list
  argList = as.list(call_standardize(distExpr)[-1])
  argNames = names(argList)
  argValues = as.character(argList)

  paramIndices = which(names(argList)=="")
  argIndices = which(!(names(argList)==""))

  argDF = data.frame(argName = argNames[argIndices],
                       argValue = argValues[argIndices],
                       stringsAsFactors = FALSE)

  paramDF = data.frame(argName = argValues[paramIndices],
                       argValue = argValues[paramIndices],
                       stringsAsFactors = FALSE)

  z = list(
    distr = TRUE,
    fcn = fnName,
    paramDF = paramDF,
    argDF = argDF
  )
  return(z)
}


### function to decompose rhs argument
### if distribution, call rhsDecompDistr
### if formula, call rhsDecompFormula
## read in expression as a quosure
rhsDecomp = function(rhs) {
  distExpr = rlang::enexpr(rhs)
  simpleRHS = FALSE  ## assume complex expression

  ## handle cases where just distribution name is supplied
  ## if function in causact namespace other than below, assume distr
  notDistrFunctions = c("%*%","eigen","iprobit","ilogit","colMeans","apply","abind","icloglog","icauchit","log1pe","imultilogit","zeros","ones","as_data","icloglog","icauchit","log1pe","imultlogit")

  if (is.symbol(distExpr)) {
    fnName = rlang::as_string(distExpr)
    if (fnName %in% getNamespaceExports("causact") &
        !(fnName %in% notDistrFunctions)) {
      ## it is a causact distribution - add parantheses so not symbol
      distExpr = rlang::parse_expr(paste0(fnName, "()"))
    } else {
      distExpr = rlang::parse_expr(fnName)
      if (!(fnName %in% getNamespaceExports("causact") &
          !(fnName %in% notDistrFunctions))) {
      simpleRHS = TRUE } ##expression is not complex
    }
  }

  ## handle cases where namespace is used
  distString = rlang::expr_text(distExpr)
  if (startsWith(distString,"causact::")) {
    distString = gsub("causact::","",distString)
    distExpr = rlang::parse_expr(distString)
    if(is.symbol(distExpr)) {  ## if now symbol, add parantheses
      distExpr = rlang::parse_expr(paste0(rlang::as_string(distExpr),"()"))
    }
  }

  ## handle cases where simple numeric constant is used
  if(is.numeric(distExpr)) {
    simpleRHS = TRUE
    fnName = NA}

  ## standardize the call
  if(!simpleRHS) {distExpr = call_standardize(distExpr)}

  ## return function name
  if(!simpleRHS) {fnName = rlang::call_name(distExpr)}

  if (fnName %in% getNamespaceExports("causact") &
      !(fnName %in% notDistrFunctions)) {
    if (fnName %in% c("mixture","joint")) {
      z = rhsDecompMixJointDistr(!!distExpr)
      } else {  ##standard distribution
        z = rhsDecompDistr(!!distExpr)
      }
  } else {
    z = rhsDecompFormula(!!distExpr)
  }

  return(z)
}


# ##testing code
# tmp(normal)
# tmp(causact::normal)
# tmp(normal(mean = 2, sd = 3))
# tmp(normal(
#   mean = 2,
#   sd = 3,
#   truncation = c(0, Inf)
# ))
# tmp(normal(mean = mu, sd = sigma))
# tmp(normal(mu, sigma))
# tmp(normal(mean = mu, sigma))
# tmp(normal(sd = sigma, mean))
# tmp(rhs = alpha + beta * x)
# tmp(rhs = beta * x)
#

### get prior composition
rhsPriorComposition = function(graph) {

  ## get nodes which have prior information
  nodeDF = graph$nodes_df %>%
    dplyr::filter(distr == TRUE) %>%
    select("id",rhs,rhsID)

  ## retrieve non-NA argument list
  argDF = graph$arg_df %>%
    dplyr::filter(!is.na(argValue))

  ## get plate information for dim argument of priors
  plateDimDF = graph$plate_index_df %>%
    dplyr::filter(!is.na(dataNode)) %>% ##only plates with data
    dplyr::left_join(graph$plate_node_df, by = "indexID") %>%
    dplyr::select(nodeID,indexLabel)

  ## create label for the rhs for these nodes
  auto_rhsDF = nodeDF %>% dplyr::left_join(argDF, by = "rhsID") %>%
    dplyr::mutate(argValue = ifelse(is.na(argDimLabels),argValue,
                                    paste0(argValue,"[",
                                           ifelse(stringr::str_detect(argDimLabels,","),
                                                  paste0("cbind(", argDimLabels,")"),
                                                  argDimLabels),  ## use cbind for R indexing
                                           "]"))) %>% ## add extraction index to label
    dplyr::group_by(.data$id,rhsID,rhs) %>%
    dplyr::summarize(args = paste0(argName," = ",argValue,collapse = ", "), .groups = "drop_last") %>%
    dplyr::left_join(plateDimDF, by = c("id" = "nodeID")) %>%
    dplyr::mutate(indexLabel = ifelse(is.na(indexLabel) | indexLabel == "NA","",indexLabel)) %>%
    dplyr::mutate(indexLabel = ifelse(indexLabel == "","",paste0(indexLabel,"_dim"))) %>%
    dplyr::group_by(.data$id,rhsID,rhs,args) %>%
    dplyr::summarize(indexLabel = paste0(indexLabel, collapse = ","), .groups = "drop_last") %>%
    dplyr::mutate(indexLabel = ifelse(stringr::str_detect(indexLabel,","),
                                      paste0("c(",indexLabel,")"),
                                      indexLabel)) %>%
    dplyr::mutate(indexLabel = ifelse(indexLabel == "",as.character(NA),indexLabel)) %>%
    dplyr::mutate(prior_rhs = paste0(rhs,"(",args,
                                    ifelse(is.na(indexLabel),"",
                                           paste0(", dim = ",indexLabel)),
                                    ")")) %>%
    dplyr::ungroup() %>%
    select("id",prior_rhs)

  ##update graph with new label
  graph$nodes_df = graph$nodes_df %>% left_join(auto_rhsDF, by = "id") %>%
    mutate(auto_rhs = ifelse(is.na(prior_rhs),auto_rhs,prior_rhs)) %>%
    dplyr::select(-prior_rhs)

  return(graph) ##now has populated graph$nodes_df$auto_rhs for priors
}

### if formula grab rhs, add dimLabels, and output in auto_rhs
rhsOperationComposition = function(graph) {
  graph$nodes_df$auto_rhs = ifelse(is.na(graph$nodes_df$auto_rhs) & graph$nodes_df$distr == FALSE & !is.na(graph$nodes_df$rhs),
                                   graph$nodes_df$rhs,
                                   graph$nodes_df$auto_rhs)

  ## update auto_rhs with dimensioned formulas
  ## get nodes with argDimLabels
  argDimLabelNodes = graph$arg_df %>%
    dplyr::filter(!is.na(argDimLabels)) %>%
    select(rhsID,argName,argDimLabels)

  ## only do if there arguments needing dim labels added
  if(nrow(argDimLabelNodes) > 0) { ##start if

  for(i in 1:nrow(argDimLabelNodes)){
    ### find index of node with matching rhsID
    nodePosition = which(graph$nodes_df$rhsID == argDimLabelNodes$rhsID[i])
    ### replace the string if it is a formula
    if(graph$nodes_df$distr[nodePosition] == FALSE &
       !is.na(graph$nodes_df$auto_rhs[nodePosition])) {
         graph$nodes_df$auto_rhs[nodePosition] =
           stringr::str_replace(graph$nodes_df$auto_rhs[nodePosition],
                                argDimLabelNodes$argName[i],
                                paste0(argDimLabelNodes$argName[i],
                                       "[",
                                       argDimLabelNodes$argDimLabels[i],
                                       "]"))
       }
  }  ## end for loop
  } ## end if

  return(graph)
}



### simplified function to pad an abbreviated label with whitespace
### to the right.
abbrevLabelPad <- function(stringVector) {
  maxStrWidth = max(nchar(stringVector), 6)
  padding = paste(rep(" ", maxStrWidth), collapse = "")
  paddedString = paste0(stringVector, padding)
  string = substr(paddedString, 1, maxStrWidth)
  return(string)
}

## function to get just column name from data
dfColToJustCol = function(data) {
  splitMatrix = stringr::str_split(data, "\\$", simplify = TRUE)
  z = splitMatrix[, ncol(splitMatrix)]
  return(z)
}

## function to find all nodeIDS where nodeLabel might be a match
## this is to give users flexibility in using labels or descriptions
## or ID's to reference a node
## precedence is auto_label, auto_descr, auto_data, data
## if duplicates find the one with the highest nodeID
## (i.e. most recently created id) output is vector of node ID's
findNodeID = function(graphListOrDF, nodeLabel) {
  nodeID = vector(mode="integer", length = length(nodeLabel))
  ## if id's are numeric, assume they are correct
  if(is.numeric(nodeLabel)) {return(nodeLabel)}
  if(is.data.frame(graphListOrDF)) { # assume it is a nodeDF
    nodeDF = graphListOrDF
  } else {  ### assume it is a list (i.e. causact_graph)
    nodeDF = graphListOrDF$nodes_df
  }

  for (i in seq_along(nodeLabel)) {
    nodeID[i] = as.integer(NA) ## set id to zero meaning unlabelled yet
    # search auto_label column
    nodePosition = max(which(nodeDF$auto_label == nodeLabel[i]), 0)
    if (nodePosition > 0) {
      nodeID[i] = nodeDF$id[nodePosition]
      next
    }
    # search auto_descr column
    nodePosition = max(which(nodeDF$auto_descr == nodeLabel[i]), 0)
    if (nodePosition > 0) {
      nodeID[i] = nodeDF$id[nodePosition]
      next
    }
    # search auto_data column
    nodePosition = max(which(nodeDF$auto_data == nodeLabel[i]), 0)
    if (nodePosition > 0) {
      nodeID[i] = nodeDF$id[nodePosition]
      next
    }
    # search data column
    nodePosition = max(which(nodeDF$data == nodeLabel[i]), 0)
    if (nodePosition > 0) {
      nodeID[i] = nodeDF$id[nodePosition]
      next
    }
    # return NA
    errMessage = paste0(nodeLabel[i],
                        " is not found as a label, description, or data for a node.")
    print(errMessage)
    nodeID[i] = as.integer(NA)
  }
  return(nodeID)
}

##get number of rows from string vector or data frame
getRowDim = function(exprArg, nLevelsUp = 1){
  exprArg = rlang::parse_expr(exprArg)
  df = rlang::eval_tidy(exprArg,env = rlang::caller_env(nLevelsUp))
  z = NROW(df)
  return(z)
}

##get number of columns from string vector or data frame
getColDim = function(exprArg, nLevelsUp = 1){
  exprArg = rlang::parse_expr(exprArg)
  df = rlang::eval_tidy(exprArg,env = rlang::caller_env(nLevelsUp))
  z = NCOL(df)
  return(z)
}

##get number of unique values from string vector
getPlateDim = function(exprArg, nLevelsUp = 1){
  exprArg = rlang::parse_expr(exprArg)
  evalVector = rlang::eval_tidy(exprArg,env = rlang::caller_env(nLevelsUp))
  z = length(unique(evalVector))
  return(z)
}

## since Diagrammer only supports one cluster/subgraph
## create pseudo plateDF and plateNodeDF that makes unique
## clusters for every combination of plates
pseudoPlate = function(graph) {
  nodeDF = graph$nodes_df
  edgeDF = graph$edges_df
  argDF = graph$arg_df
  plateDF = graph$plate_index_df
  plateNodeDF = graph$plate_node_df


  #####START PLATE NOTATION CODE########
  ###update graph cluster information for plates
  ##### Code to render plate notation subgraphs when
  ##### there are either intersecting or nested subgraphs

  pseudo_plate_index_df = plateDF
  pseudo_plate_node_df = plateNodeDF


  ## is any node duplicated (returns index of first duplicate node)
  duplicatePosition = anyDuplicated(plateNodeDF$nodeID)

  ## if node appears twice, need to make a virtual cluster
  ## such that each node is a member of a unique subgraph
  ## composed of all of the node's indexes

  while (duplicatePosition > 0) {
    duplicatedNodeID = plateNodeDF$nodeID[duplicatePosition]

    ## get vector of indexID's for node
    indices = plateNodeDF$indexID[plateNodeDF$nodeID == duplicatedNodeID]

    ## make new combined plate for multi-index node
    newIndex = max(plateNodeDF$indexID) + 1
    newIndexLabel = paste(plateDF$indexLabel[indices], collapse = "")
    newIndexDescr = paste(plateDF$indexDescription[indices], collapse = "_")
    newIndexDispName = paste0(
      paste0(
        plateDF$indexDisplayName[indices],
        collapse = "\\r"
      ),
      "\\r"
    )
    newRow = data.frame(
      indexID = newIndex,
      indexLabel = newIndexLabel,
      indexDescription = newIndexDescr,
      indexDisplayName = newIndexDispName,
      stringsAsFactors = FALSE
    )

    pseudo_plate_index_df = dplyr::bind_rows(plateDF, newRow)

    ## make nodes point to new plate
    pseudo_plate_node_df = plateNodeDF %>%
      dplyr::filter(nodeID != duplicatedNodeID) %>%
      dplyr::bind_rows(data.frame(indexID = newIndex, nodeID = duplicatedNodeID))

    ## update plateDF and plateNodeDF
    plateNodeDF = pseudo_plate_node_df
    plateDF = pseudo_plate_index_df

    ## condition to break out.
    duplicatePosition = anyDuplicated(pseudo_plate_node_df$nodeID)
  }  #end while loop


   graph$nodes_df = nodeDF
   graph$edges_df = edgeDF
   graph$arg_df = argDF
   graph$plate_index_df = pseudo_plate_index_df
   graph$plate_node_df = pseudo_plate_node_df

   return(graph)
}


### function to update edgeDF with extract edges
updateExtractEdges = function(graphWithDim) {
  graph = graphWithDim
  ## get candidate edges for being extract edges
  edgeDF = graph$edges_df %>%
    dplyr::filter(is.na(type) | type == "extract")
  extractCandidateDF = edgeDF %>%
    mutate(candidate = as.logical(NA))
  ## make df of plate indexes for each from and to edge
  if(nrow(edgeDF) > 0) {
    ### plate indices for all from nodes
    edgeDFFrom = edgeDF %>%
      dplyr::left_join(graph$plate_node_df, by = c("from" = "nodeID")) %>%
      dplyr::select(from,indexID) %>%
      dplyr::distinct() %>%
      dplyr::group_by(from) %>%
      tidyr::nest(fromPlateIndices = indexID)
    ### plate indices for all to nodes
    edgeDFTo = edgeDF %>%
      dplyr::left_join(graph$plate_node_df, by = c("to" = "nodeID"))  %>%
      dplyr::select(to,indexID) %>%
      dplyr::distinct() %>%
      dplyr::group_by(to) %>%
      tidyr::nest(toPlateIndices = indexID)

    ### compare plate indices for from and to
    if(nrow(extractCandidateDF) > 0) {
    for (i in 1:nrow(extractCandidateDF)) {
      fromNode = extractCandidateDF$from[i]
      toNode = extractCandidateDF$to[i]

      fromPosition = which(edgeDFFrom$from == fromNode)
      toPosition = which(edgeDFTo$to == toNode)

      fromIndices = unique(unlist(edgeDFFrom$fromPlateIndices[fromPosition]))
      toIndices = unique(unlist(edgeDFTo$toPlateIndices[toPosition]))
      if(setequal(fromIndices,toIndices)) { ## same plate, no extract
        extractCandidateDF$candidate[i] = FALSE } else if (length(setdiff(fromIndices,toIndices)[1]) > 0 & !is.na(setdiff(fromIndices,toIndices)[1])) {  ##from plate has index not on child plate
          extractCandidateDF$candidate[i] = TRUE } else {  ## child on all plates of parent
            extractCandidateDF$candidate[i] = FALSE
          }

    } #end for
    } #end if
  } #end if

  ### for now, assume all candidates get arguments updated
  ### 1) Create extract edge in edgeDF
  ### 2) Update argDimDF
  extractNodes = extractCandidateDF %>%
    dplyr::filter(.data$candidate == TRUE) %>%
    dplyr::pull(.data$id)

  edgeDF$type[edgeDF$id %in% extractNodes] <- "extract"

  graph$edges_df = graph$edges_df %>%  ### add back edges that are not extract candidates
    dplyr::union(edgeDF) %>%
    dplyr::arrange(type) %>%
    dplyr::distinct(.data$id,from,to,.keep_all = TRUE)  ##get rid of type = NA edges that were found to be extract candidates

  return(graph)

}

### function to update argDF with dimensions of extract edges
updateExtractArguments = function(graphWithDim) {
  graph = graphWithDim
  rowsToAddtoArgDF = graph$edges_df %>%
    dplyr::filter(type == "extract") %>%
    dplyr::select(from,to) %>% ## get extraction edges
    dplyr::left_join(graph$nodes_df, by = c("from" = "id")) %>%
    dplyr::select(from, to, fromLabel = auto_label) %>%
    dplyr::left_join(graph$plate_node_df, by = c("from" = "nodeID")) %>%
    dplyr::left_join(graph$plate_index_df, by = "indexID") %>%
    dplyr::select(from,to,fromLabel,dimLabel = indexLabel) %>% ## done with from node info
    dplyr::left_join(graph$nodes_df, by = c("to" = "id")) %>%
    dplyr::select(from,to,fromLabel,dimLabel,rhsID) %>%
    dplyr::left_join(graph$arg_df, by = c("rhsID" = "rhsID","fromLabel" = "argValue")) %>%
    select(rhsID,argName,argType,argValue = fromLabel,argDimLabels = dimLabel)  ##new dim label

  ## update if nrow >= 1
  if (nrow(graph$arg_df) >= 1) {
    graph$arg_df = dplyr::bind_rows(graph$arg_df, rowsToAddtoArgDF) %>%
      dplyr::mutate(argID = row_number()) %>%  ## retain order of arguments
      dplyr::group_by(rhsID, argName, argType, argValue) %>%
      dplyr::summarize(
        argDimLabels = paste0(argDimLabels, collapse = ","),
        minRowID = min(argID), .groups = "drop_last"
      ) %>%
      dplyr::mutate(argDimLabels = gsub("NA,", "", argDimLabels)) %>%
      dplyr::mutate(argDimLabels = ifelse(argDimLabels == "NA", as.character(NA), argDimLabels)) %>%
      dplyr::arrange(rhsID, minRowID) %>%
      dplyr::select(-minRowID) %>%
      as.data.frame()
  }

  return(graph)
}


##helper function to get the level names of a factor
##in the global environment
getLevelNames = function(dataNode) {
  nameVector = rlang::eval_tidy(rlang::parse_expr(
    paste0("levels(as.factor(",
           dataNode,
           "))")
  ),
  env = rlang::global_env())
  return(nameVector)
}


##function to make diagonal matrix given vector of diagonal elements

makeDiagMatrix <- function(diagVec) {
  if (!is.vector(diagVec)) {
    stop("Input 'diagVec' must be a vector.")
  }

  n <- length(diagVec)
  diag_matrix <- diag(diagVec)

  return(diag_matrix)
}


# below two functions allow one to get objectName
# that becomes argument
# see https://michaelbarrowman.co.uk/post/getting-a-variable-name-in-a-pipeline/
get_lhs <- function(){
  calls <- sys.calls()

  #pull out the function or operator (e.g. the `%>%`)
  call_firsts <- lapply(calls,`[[`,1)

  #check which ones are equal to the pipe
  pipe_calls <- vapply(call_firsts,identical,logical(1),quote(`%>%`))

  #if we have no pipes, then get_lhs() was called incorrectly
  if(all(!pipe_calls)){
    NULL
  } else {
    #Get the most recent pipe, lowest on the
    pipe_calls <- which(pipe_calls)
    pipe_calls <- pipe_calls[length(pipe_calls)]

    #Get the second element of the pipe call
    this_call <- calls[[c(pipe_calls,2)]]

    #We need to dig down into the call to find the original
    while(is.call(this_call) && identical(this_call[[1]],quote(`%>%`))){
      this_call <- this_call[[2]]
    }
    this_call

  }
}
# see https://michaelbarrowman.co.uk/post/getting-a-variable-name-in-a-pipeline/
get_name <- function(x){
    lhs <- get_lhs()
    if(is.null(lhs)){
      lhs <- rlang::ensym(x)
    }
    rlang::as_name(lhs)
}

## take this function from rlang as they are deprecating it
## change from standardise to standardize
call_standardize <- function(call, env = caller_env()) {

  expr <- rlang::get_expr(call)
  if (!is_call(expr)) {
    abort_call_input_type("call")
  }

  # The call name might be a literal, not necessarily a symbol
  env <- rlang::get_env(call, env)
  fn <- rlang::eval_bare(rlang::node_car(expr), env)

  if (rlang::is_primitive(fn)) {
    call
  } else {
    matched <- match.call(fn, expr)
    rlang::set_expr(call, matched)
  }
}

## adapted from rlang - avoid using cli like rlang does
.style_inline <- function(x, span, fallback = "`%s`") {
  if (is.null(fallback)) {
    x
  } else if (is.function(fallback)) {
    fallback(x)
  } else {
    sprintf(fallback, x)
  }
}
.format_inline <- function(x, span, fallback = "`%s`") {
    .style_inline(x, span, fallback = fallback)
}
format_arg    <- function(x) .format_inline(x, "arg", "`%s`")
abort_call_input_type <- function(arg, call = caller_env()) {
  abort(
    sprintf("%s must be a quoted call.", format_arg(arg)),
    call = call
  )
}

## install utility function
#' Check if 'r-causact' Conda environment exists
check_r_causact_env <- function() {
  tryCatch({
    env_list <- reticulate::conda_list()
    result <- "r-causact" %in% env_list$name
  }, error = function(e) {
    result <- FALSE
  })
}

# Function to replace 'c(' with 'concatenate(atleast_1d(' and ')' with '))'
replace_c <- function(input_string) {
  pattern <- "(^|\\s)c\\((.*?)\\)"
  replacement <- "\\1concatenate(atleast_1d(\\2))"
  modified_string <- gsub(pattern, replacement, input_string, perl = TRUE)
  return(modified_string)
}

