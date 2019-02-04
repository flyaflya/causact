#' Create a graph object focused on drawing a DAG.
#'
#' Generates a \code{dgr_graph} graph object that is set-up for drawing DAG graphs.
#' @param ... Any arguments to pass to \code{DiagrammeR::create_graph()}.
#' @examples
#' # With `dag_create()` we can
#' # simply create an empty graph (and
#' # add in nodes and edges later
#' # with other functions)
#' dag_create() %>% dag_render(shortLabel = TRUE)
#' @importFrom dplyr bind_rows tibble
#' @importFrom DiagrammeR create_graph add_global_graph_attrs
#' @export
dag_create <- function(...) {

  ### create graph
  graph = DiagrammeR::create_graph(directed = TRUE)

  # Add custom attributes for DAG building
  graph$nodes_df$description[-1] = as.character(NA)
  graph$nodes_df$distr[-1] = as.character(NA)
  graph$nodes_df$formulaString[-1] = as.character(NA)
  graph$nodes_df$fullDistLabel[-1] = as.character(NA)
  graph$nodes_df$data[-1] = as.character(NA)
  graph$nodes_df$userSpecifiedArgs[-1] = as.logical(NA)
  graph$nodes_df$gretaRHS[-1] = as.character(NA)

  ## add two df's to store plate index information
  # Create an empty index data frame (`idf`)
  pidf <-
    data.frame(
      indexID = as.integer(NA),
      indexLabel = as.character(NA),
      indexDescription = as.character(NA),
      indexDisplayName = as.character(NA),
      dataNode = as.character(NA),
      stringsAsFactors = FALSE)[-1, ]

  # Create an empty plate nodes data frame (`pdf`)
  pndf <-
    data.frame(
      indexID = as.integer(NA),
      nodeID = as.integer(NA),
      stringsAsFactors = FALSE)[-1, ]

  graph$plate_index_df = pidf
  graph$plate_nodes_df = pndf

  ###update global graph attributes
  graph = graph %>%
    DiagrammeR::add_global_graph_attrs(attr = "layout",
                                       value = "dot",
                                       attr_type = "graph") %>%
    DiagrammeR::add_global_graph_attrs(attr = "fillcolor",
                                       value = "AliceBlue",
                                       attr_type = "node") %>%
    DiagrammeR::add_global_graph_attrs(attr = "style",
                                       value = "filled",
                                       attr_type = "node") %>%
    DiagrammeR::add_global_graph_attrs(attr = "shape",
                                       value = "ellipse",
                                       attr_type = "node") %>%
    DiagrammeR::add_global_graph_attrs(attr = "height",
                                       value = "0.5",
                                       attr_type = "node") %>%
    DiagrammeR::add_global_graph_attrs(attr = "width",
                                       value = "0.9",
                                       attr_type = "node") %>%
    DiagrammeR::add_global_graph_attrs(attr = "fixedsize",
                                       value = "false",
                                       attr_type = "node")  %>%
    DiagrammeR::add_global_graph_attrs(attr = "margin",
                                       value = "0.05,0.05",
                                       attr_type = "node") %>%
    DiagrammeR::add_global_graph_attrs(attr = "color",
                                       value = "gray20",
                                       attr_type = "node")  %>%
    DiagrammeR::add_global_graph_attrs(attr = "color",
                                       value = "gray20",
                                       attr_type = "edge")  %>%
    DiagrammeR::add_global_graph_attrs(attr = "fontcolor",
                                       value = "black",
                                       attr_type = "node") %>%
    DiagrammeR::add_global_graph_attrs(attr = "fontcolor",
                                       value = "black",
                                       attr_type = "edge") %>%
    DiagrammeR::add_global_graph_attrs(attr = "labelloc",
                                       value = "b",
                                       attr_type = "graph") %>%
    DiagrammeR::add_global_graph_attrs(attr = "labeljust",
                                       value = "r",
                                       attr_type = "graph")

  return(graph)
}
