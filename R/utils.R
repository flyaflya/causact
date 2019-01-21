# Function that gets the calling function
# as a formatted character string
#' @importFrom stringr str_replace_all
get_calling_fcn <- function() {

  calling_fcn <- deparse(sys.call(-1))

  stringr::str_replace_all(
    calling_fcn,
    pattern = "([a-z0-9_]*)(.*)",
    replacement = "\\1")
}


# Function to add log line for a graph `action`
#' @importFrom dplyr bind_rows
add_action_to_log <- function(graph_log,
                              version_id,
                              function_used,
                              time_modified,
                              duration,
                              nodes,
                              edges,
                              d_n = 0,
                              d_e = 0) {

  # Ensure that `time_modified` inherits from POSIXct
  if (inherits(time_modified, "POSIXct") == FALSE) {

    stop(
      "The `time_modified` value must inherit from POSIXct.",
      call. = FALSE)
  }

  # Create a log line
  graph_log_line <-
    data.frame(
      version_id = as.integer(version_id),
      function_used = as.character(function_used),
      time_modified = time_modified,
      duration = as.numeric(duration),
      nodes = as.integer(nodes),
      edges = as.integer(edges),
      d_n = as.integer(d_n),
      d_e = as.integer(d_e),
      stringsAsFactors = FALSE)

  # Append the log line to `graph_log`
  dplyr::bind_rows(graph_log, graph_log_line)
}

# Function to get the time of the graph function in
# the user's locale
graph_function_sys_time <- function() {
  return(Sys.time())
}

# Function to get the time difference from the start
# of the function (relies on a call of the
# `graph_function_sys_time()` function) to the time
# of invoking this function
graph_function_duration <- function(start_time) {
  end_time <- Sys.time()
  time_diff_s <- (end_time - start_time)[[1]]
  return(time_diff_s)
}


###
# Graph validation functions
###

# Function to check whether a graph object is valid
graph_object_valid <- function(graph) {

  # Check for all component names to be present
  if (!all(c("graph_info", "nodes_df", "edges_df",
             "global_attrs", "directed",
             "last_node", "last_edge",
             "node_selection", "edge_selection",
             "cache", "graph_log") %in%
           names(graph))) {

    return(FALSE)
  }

  # Check for specific graph classes
  if (any(
    inherits(graph$graph_info, "data.frame") == FALSE,
    inherits(graph$nodes_df, "data.frame") == FALSE,
    inherits(graph$edges_df, "data.frame") == FALSE,
    inherits(graph$global_attrs, "data.frame") == FALSE,
    inherits(graph$global_attrs$attr, "character") == FALSE,
    inherits(graph$global_attrs$value, "character") == FALSE,
    inherits(graph$global_attrs$attr_type, "character") == FALSE,
    inherits(graph$directed, "logical") == FALSE,
    inherits(graph$node_selection, "data.frame") == FALSE,
    inherits(graph$edge_selection, "data.frame") == FALSE,
    inherits(graph$cache, "list") == FALSE,
    inherits(graph$graph_log, "data.frame") == FALSE)) {

    return(FALSE)
  }

  return(TRUE)
}

### function that takes greta dist input from user and returns
### a list of three strings:
### 1) $distString:  name of distributino
### 2) $distArgs: string of comma spearated arguments
### 3) $fullDistLable: label to use in DAG regarding the distribution
### function that takes input from user and returns
### a fully formatted label regarding the distribution
getFullDistList = function(distr = greta::variable) {

  distr = rlang::enexpr(distr) ## take in argument as expression
  distString = as.character(distr)  ### make into string
  userSpecifiedArgs = TRUE  ### assume user supplied arguments
  distArgs = NA  ###initialize as NA

  ## if it is a greta distribution, get default arguments
  ## when not greta object (e.g. a function like normal(6,2))
  ## the below code generates errors that are ignored
  tryCatch({
    distString = tail(as.character(distr), n = 1) ## function name
    distArgs = formalArgs(eval(distr))  ##function arguments
    if(is.character(distArgs)) {userSpecifiedArgs = FALSE}  ## if in this path, arguments not specified
    numParents = which(distArgs == "dim") - 1  ## get number of dist paramaters
    distArgs = paste0(distArgs[1:numParents], collapse = ",")  ## keep only dist parameters
  }, warning = function(w){})

  if (userSpecifiedArgs == TRUE){
    distString = as.character(distr)  ### make into string
    distArgs = paste0(distString[2:length(distString)],collapse =",")
    distString = gsub("greta::","",distString[1])
  }
  fullDistLabel = paste0(distString,"(",distArgs,")")

  return(list(distString = distString, distArgs = distArgs, fullDistLabel = fullDistLabel))
}
