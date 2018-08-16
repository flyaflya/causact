#' Set DiagrammeR defaults for graphical models
#'
#' \code{setDirectedGraph} returns a graph with good defaults.
#'
#' @param dgrGraph A DiagrammeR graph
#' @return An updated version of \code{dgrGraph} with good defaults for
#'   graphical models.
#' @examples
#' graphA %>% setDirectedGraphTheme() %>% render_graph()
#' @importFrom DiagrammeR add_global_graph_attrs
#' @export
setDirectedGraphTheme = function(dgrGraph) {
  ## set global attributes
  tempGR = dgrGraph %>%
    DiagrammeR::add_global_graph_attrs(attr = "layout",
                                       value = "dot",
                                       attr_type = "graph") %>%
    DiagrammeR::add_global_graph_attrs(attr = "fillcolor",
                                       value = "AliceBlue",
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
                                       attr_type = "edge")
  ## update attributes for specific node types
  n_nodes <- nrow(tempGR$nodes_df)
  tempGR$nodes_df$fillcolor = rep("aliceblue", n_nodes)
  tempGR$nodes_df$fillcolor[tempGR$nodes_df$type == "obs"] = "cadetblue"

  ## correct for fontcolor bug
  tempGR$nodes_df$fontcolor = "black"

  ## return updated graph
  return(tempGR)
}

setWorkflowTheme = function(dgrGraph) {
  dgrGraph %>%
    DiagrammeR::add_global_graph_attrs(attr = "layout",
                                       value = "dot",
                                       attr_type = "graph") %>%
    DiagrammeR::add_global_graph_attrs(attr = "rankdir",
                                       value = "LR",
                                       attr_type = "graph") %>%
    DiagrammeR::add_global_graph_attrs(attr = "shape",
                                       value = "plain",
                                       attr_type = "node") %>%
    DiagrammeR::add_global_graph_attrs(attr = "fixedsize",
                                       value = "false",
                                       attr_type = "node")  %>%
    DiagrammeR::add_global_graph_attrs(attr = "margin",
                                       value = "0.05,0.05",
                                       attr_type = "node") %>%
    DiagrammeR::add_global_graph_attrs(attr = "fillcolor",
                                       value = "white",
                                       attr_type = "node") %>%
    DiagrammeR::add_global_graph_attrs(attr = "color",
                                       value = "gray20",
                                       attr_type = "edge")
}

