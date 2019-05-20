#' Add an edge between nodes in a graph object.
#'
#' With a graph object of class \code{causact_graph} created from \code{dag_create}, add an edge between nodes in the graph.  Vector recycling is used for all arguments.
#' @param from a character vector representing the parent nodes label or description from which the edge is connected.
#' @param to the child node label or description from which the edge is connected.
#' @param type character string used to represent the DiagrammeR line type (e.g. \code{"solid"}).  Use \code{type = "extract"} to encourage causact to only pass indexed elements of the parent node to each instance of the child node.  Specify \code{type = "solid"} to override any automated extract behavior.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 2 connected nodes
#' dag_create() %>%
#'   dag_node("X") %>%
#'   dag_node("Y") %>%
#'   dag_edge(from = "X", to = "Y") %>%
#'   dag_render(shortLabel = TRUE)
#' @importFrom dplyr bind_rows
#' @export
dag_edge <- function(graph,
                     from,
                     to,
                     type = as.character(NA)) {

  ## First validate that the first argument is indeed a causact_graph
  class_g <- class(graph)
  ## Any causact_graph will have class length of 1
  if(length(class_g) > 1){
    ## This specific case is hard-coded as it has occured often in early use by the author
    if(class_g[1] == chr("grViz") && class_g[2]=="htmlwidget"){
      errorMessage <- paste0("Given rendered Causact Graph. Check the declaration for a dag_render() call.")
    }
    else {
      errorMessage <- paste0("Cannot add dag_edge() to given object as it is not a Causact Graph.")
    }
    stop(errorMessage)
  }
  ## Now check the single class
  if(class_g != "causact_graph"){
    errorMessage <- paste0("Cannot add dag_edge() to given object as it is not a Causact Graph.")
    stop(errorMessage)
  }

  ## Check that we maintain a DAG
  ## That that the edge being created is not to the same node.
  if(anyDuplicated(c(from,to))){
    errorMessage <- paste("You have attempted to connect", c(from,to)[anyDuplicated(c(from,to))],"to itself, but this would create a cycle and is not a Directed Acyclic Graph. You cannot connect a node to itself in a DAG.")
    stop(errorMessage)
  }

  ## extract nodeIDs
  ## checking that the 'from' and 'to' nodes exist in the graph.
  fromIDs = findNodeID(graph,from)
  if(anyNA(fromIDs)){
    errorMessage <- paste("Node",from,"does not exist. Check for spelling errors. Check the order the nodes were created!")
    stop(errorMessage)
  }

  toIDs = findNodeID(graph,to)
  if(anyNA(toIDs)){
    errorMessage <- paste("Node",to,"does not exist. Check for spelling errors. Check the order the nodes were created!")
    stop(errorMessage)
  }

  ## get number of edges
  numberOfEdges = max(length(from),length(to))
  ## if no edges, simply return graph
  if(numberOfEdges == 0) {return(graph)}

  ## initialize edgeDF info for this edge(s)
  edgeIDstart = max(graph$edges_df$id,0) + 1
  edf = data.frame(
    id = edgeIDstart:(edgeIDstart+numberOfEdges-1),
    from = fromIDs,
    to = toIDs,
    type = type,
    stringsAsFactors = FALSE
  )

  graph$edges_df = dplyr::bind_rows(graph$edges_df,edf)

  return(graph)
}


