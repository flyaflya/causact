#' Set DiagrammeR defaults for graphical models
#'
#' `setDirectedGraph` returns a graph with good defaults.
#'
#' @param dgrGraph A DiagrammeR graph
#' @param fillColor Default R color for filling nodes.
#' @param fillColorObs R color for filling obeserved nodes.
#' @return An updated version of `dgrGraph` with good defaults for
#'   graphical models.
#' @examples
#' library(DiagrammeR)
#' create_graph() %>% add_node() %>% render_graph()  # default DiagrammeR aesthetics
#' create_graph() %>% add_node() %>% setDirectedGraphTheme() %>% render_graph() ## causact aesthetics
#' @importFrom DiagrammeR add_global_graph_attrs
#' @return return a `dgrGraph` object with the color and shape defaults used by the `causact` package.
#' @export

setDirectedGraphTheme = function(dgrGraph, fillColor = "aliceblue", fillColorObs = "cadetblue") {
  ## set global attributes
  tempGR = dgrGraph %>%
    DiagrammeR::add_global_graph_attrs(attr = "layout",
                                       value = "dot",
                                       attr_type = "graph") %>%
    DiagrammeR::add_global_graph_attrs(attr = "fillcolor",
                                       value = "AliceBlue",
                                       attr_type = "node") %>%
    DiagrammeR::add_global_graph_attrs(attr = "style",
                                       value = "filled",
                                       attr_type = "node")%>%
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

  ## update attributes for specific node types
  n_nodes <- nrow(tempGR$nodes_df)
  tempGR$nodes_df$fillcolor = rep(fillColor, n_nodes)
  tempGR$nodes_df$fillcolor[tempGR$nodes_df$type == "obs"] = fillColorObs
  tempGR$nodes_df$fillcolor[tempGR$nodes_df$type == "censObs"] = "aliceblue;0.5:cadetblue"
  #tempGR$nodes_df$shape[tempGR$nodes_df$shape == "rect"] = "rect"

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

