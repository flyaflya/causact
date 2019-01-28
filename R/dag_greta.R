#' Create a graph object focused on drawing a DAG.
#'
#' The graph object should be of class \code{dgr_graph} and created using \code{dag_create()}.
#' @param graph a graph object of class \code{dgr_graph} created using \code{dag_create()} with data argument populated.
#' @examples
#' # With `dag_create()` we can
#' # simply create an empty graph (and
#' # add in nodes and edges later
#' # with other functions)
#' dag_create() %>% dag_render(shortLabel = TRUE)
#' @importFrom dplyr bind_rows tibble
#' @importFrom DiagrammeR create_graph add_global_graph_attrs
#' @importFrom rlang enquo expr_text
#' @export
dag_greta <- function(graph) {
  ###Extract relevant graph objects
  nodesDF = graph$nodes_df
  edgesDF = graph$edges_df
  plateDF = graph$plate_index_df
  plateNodesDF = graph$plate_nodes_df

  ###Use DAPROPLIMOPO(DAta,PRior,OPeration,LIkelihood,MOdel,POsterior)
  ###Find all nodes that require data based on user input
  ###Err on the side of including a node
  lhsNodesDF = nodesDF %>%
    dplyr::filter(type == "obs" | data != "as.character(NA)")

  ###Create Code for Data Line And Place in DF
  lhsNodesDF = lhsNodesDF %>%
    mutate(dataLine = paste0(abbreviate(label),
                             " <- ",
                             "as_data(",
                             data,
                             ")  ##data")) %>%
    mutate(lhs = label)

  ###Aggregate Code Statements
  codeStatements = paste(lhsNodesDF$dataLine,
                         sep = "\n")

  ##make expression out of Code Statements
  codeExpr = parse(text = codeStatements)

  ##eval expression
  eval(codeExpr, envir = globalenv())

  ###return code
  return(codeExpr)
}
