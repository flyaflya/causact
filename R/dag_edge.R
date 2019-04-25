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

  ## get number of edges
  numberOfEdges = max(length(from),length(to))
  ## if no edges, simply return graph
  if(numberOfEdges == 0) {return(graph)}

  ## extract nodeIDs
  fromIDs = findNodeID(graph,from)
  toIDs = findNodeID(graph,to)

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


