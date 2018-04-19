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
  dgrGraph %>%
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
                                       attr_type = "edge")
}
