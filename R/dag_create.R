#' Create a graph object for drawing a DAG.
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Generates a `causact_graph` graph object that is set-up for drawing DAG graphs.
#' @examples
#' # With `dag_create()` we can create an empty graph and
#' # add in nodes (`dag_node()`), add edges (`dag_edge`), and
#' # view the graph with `dag_render()`.
#' dag_create()
#' @return a list object of class `causact_graph` consisting of 6 dataframes.  Each data frame is responsible for storing information about nodes, edges, plates, and the relationships among them.
#' @importFrom lifecycle badge
#' @export

dag_create <- function() {
  # Create an empty node data frame (`ndf`)
  ndf <-
    data.frame(
      id = as.integer(NA),
      # user entered quantities
      label = as.character(NA),
      descr = as.character(NA),
      data = as.character(NA),
      rhs = as.character(NA),#distr or formula
      child = as.character(NA),
      obs = as.logical(NA),
      rhsID = as.integer(NA),
      # auto generated labels
      distr = as.logical(NA), #true for dist / false for formula
      auto_label = as.character(NA),
      auto_descr = as.character(NA),
      auto_data = as.character(NA),
      dimID = as.integer(NA),
      auto_rhs = as.character(NA),
      stringsAsFactors = FALSE
    )[-1,]

  ## Create a DF to store RHS arguments and their values
  adf <-
    data.frame(
      rhsID = as.integer(NA),
      argName = as.character(NA),
      argType = as.character(NA), ##param or arg (i.e. non-param)
      argValue = as.character(NA),  ## can be node/object name or value
      argDimLabels = as.character(NA),
      stringsAsFactors = FALSE
    )[-1,]

  ## DF: `edges_df`

  # Create an empty edge data frame (`edf`)
  edf <-
    data.frame(
      id = as.integer(NA),
      from = as.integer(NA),
      to = as.integer(NA),
      type = as.character(NA),
      stringsAsFactors = FALSE
    )[-1, ]

  ## add two df's to store plate index information
  # Create an empty index data frame (`idf`)
  pidf <-
    data.frame(
      indexID = as.integer(NA),
      indexLabel = as.character(NA),
      indexDescription = as.character(NA),
      indexDisplayName = as.character(NA),
      dataNode = as.character(NA),
      rhs = as.character(NA),
      stringsAsFactors = FALSE
    )[-1, ]

  # Create an empty plate nodes data frame (`pdf`)
  pndf <-
    data.frame(
      indexID = as.integer(NA),
      nodeID = as.integer(NA),
      stringsAsFactors = FALSE
    )[-1, ]

  # Create an dimension data frame (`ddf`)
  ddf <-
    data.frame(
      dimID = as.integer(NA),
      nodeID = as.integer(NA),
      dimType = as.character(NA),
      dimDataSource = as.character(NA),
      dimValue = as.integer(NA),
      stringsAsFactors = FALSE
    )[-1, ]

  # Initialize a graph object
  graph <-
    list(
      nodes_df = ndf,
      edges_df = edf,
      arg_df = adf,
      plate_index_df = pidf,
      plate_node_df = pndf,
      dim_df = ddf
    )

  attr(graph, "class") <- "causact_graph"

  return(graph)
}
