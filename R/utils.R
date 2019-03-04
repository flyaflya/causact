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

  ## get top function name - probably better way to do this
  fnName = head(distString, n = 1) ## function name
  if (fnName %in% c("::", "greta")) {
    ##if namespace given
    fnName = tail(distString, n = 1)
    distString = distString[-c(1, 2)]
  }

  ## function arguments list
  allArgs = formalArgs(fnName)  ##function arguments

  ## fill in missing arguments in list
  for (i in 1:length(allArgs)) {
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

  ## sort based on standard parameter ordering for greta
  allArgsDF = data.frame(argName = allArgs, stringsAsFactors = FALSE) %>%
    dplyr::left_join(namedArgDF, by = "argName") %>%
    dplyr::select(-position)
  rm(namedArgDF)

  numParents = which(allArgs %in% c("dim", "dimension")) - 1  ## get number of dist paramaters

  paramDF = allArgsDF[1:numParents,]
  argDF = allArgsDF[(numParents + 1):nrow(allArgsDF),]
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
  fnName = deparse(formExpr)
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

### function to decompose rhs argument
### if distribution, call rhsDecompDistr
### if formula, call rhsDecompFormula
## read in expression as a quosure
rhsDecomp = function(rhs) {
  distExpr = rlang::enexpr(rhs)
  distString = as.character(distExpr)



  ## get top distr function name - probably better way to do this
  fnName = head(distString, n = 1) ## function name
  if (fnName %in% c("::", "greta")) {
    ##if namespace given
    fnName = tail(distString, n = 1)
    distString = distString[-c(1, 2)]
  }


  ## if function in greta namespace, then assume distr
  ## otherwise assume formula
  if (fnName %in% getNamespaceExports("greta")) {
    z = rhsDecompDistr(!!distExpr)
  } else {
    z = rhsDecompFormula(!!distExpr)
  }

  return(z)
}


# ##testing code
# tmp(normal)
# tmp(greta::normal)
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
## (i.e. most recently created id)
findNodeID = function(graph, nodeLabel) {
  nodeID = vector(mode="integer", length = length(nodeLabel))
  ## if id's are numeric, assume they are correct
  if(is.numeric(nodeLabel)) {return(nodeLabel)}
  nodeDF = graph$nodes_df
  for (i in 1:length(nodeLabel)) {
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
    errMessage = paste0(nodeLabel,
                        " is not found as a label, description, or data for a node.")
    print(errMessage)
    nodeID[i] = as.integer(NA)
  }
  return(nodeID)
}
