#' Create a graph object
#'
#' Generates a graph object with the option to use node data frames (ndfs)
#'   and/or edge data frames (edfs) to populate the initial graph.
#' @param nodes_df an optional data frame containing, at minimum, a column
#'   (called \code{id}) which contains node IDs for the graph. Additional
#'   columns (node attributes) can be included with values for the named node
#'   attribute.
#' @param edges_df an optional data frame containing, at minimum, two columns
#'   (called \code{from} and \code{to}) where node IDs are provided. Additional
#'   columns (edge attributes) can be included with values for the named edge
#'   attribute.
#' @param directed with \code{TRUE} (the default) or \code{FALSE}, either
#'   directed or undirected edge operations will be generated, respectively.
#' @param graph_name an optional string for labeling the graph object.
#' @param attr_theme the theme (i.e., collection of \code{graph}, \code{node},
#'   and \code{edge} global graph attributes) to use for this graph. The default
#'   theme is called \code{default}. If this is set to \code{NULL} then no
#'   global graph attributes will be applied to the graph upon creation.
#' @param write_backups an option to write incremental backups of changing
#'   graph states to disk. If \code{TRUE}, a subdirectory within the working
#'   directory will be created and used to store \code{RDS} files. The default
#'   value is \code{FALSE} so one has to opt in to use this functionality.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # With `dag_create()` we can
#' # simply create an empty graph (and
#' # add in nodes and edges later
#' # with other functions)
#' graph <- dag_create()
#'
#' # A graph can be created with
#' # nodes and without having any edges;

#' # Get information on the graph's nodes
#' graph %>%
#'   get_node_info()
#'
#' # You can create a similar graph with
#' # just nodes but also providing a
#' # range of attributes for the nodes
#' # (e.g., types, labels, or arbitrary
#' # 'values')
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     label = TRUE,
#'     type = c("type_1", "type_1",
#'              "type_5", "type_2"),
#'     shape = c("circle", "circle",
#'               "rectangle", "rectangle"),
#'     values = c(3.5, 2.6, 9.4, 2.7))
#'
#' graph <-
#'   dag_create(nodes_df = ndf)
#'
#' # Get information on the graph's
#' # internal node data frame (ndf)
#' graph %>%
#'   get_node_df()
#'
#' # A graph can also be created by
#' # specifying both the nodes and
#' # edges; create an edge data frame
#' # (edf) using the `create_edge_df()`
#' # function:
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to",
#'     values = c(7.3, 2.6, 8.3))
#'
#' # Create the graph object with
#' # `dag_create()` and pass in the
#' # ndf and edf objects
#' graph <-
#'   dag_create(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Get information on the graph's
#' # internal edge data frame (edf)
#' graph %>% get_edge_df()
#'
#' # Get information on the graph's
#' # internal node data frame (ndf)
#' graph %>% get_node_df()
#' @importFrom dplyr bind_rows tibble
#' @export
dag_create <- function(...) {

  ### create graph
  graph = DiagrammeR::create_graph(directed = TRUE)

  # Add custom attributes for DAG building
  graph$nodes_df$decription[-1] = as.character(NA)
  graph$nodes_df$distr[-1] = as.character(NA)
  graph$nodes_df$formulaString[-1] = as.character(NA)
  graph$nodes_df$fullDistLabel[-1] = as.character(NA)

  ## add two df's to store plate index information
  # Create an empty index data frame (`idf`)
  pidf <-
    data.frame(
      indexID = as.integer(NA),
      indexLabel = as.character(NA),
      indexDescription = as.character(NA),
      indexDisplayName = as.character(NA),
      stringsAsFactors = FALSE)[-1, ]

  # Create an empty plate nodes data frame (`pdf`)
  pndf <-
    data.frame(
      indexID = as.integer(NA),
      nodeID = as.integer(NA),
      stringsAsFactors = FALSE)[-1, ]

  graph$plate_index_df = pidf
  graph$plate_nodes_df = pndf

  return(graph)
}
