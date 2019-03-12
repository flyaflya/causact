#' Render the graph in various formats
#'
#' Using a \code{dgr_graph} object, render the graph in the RStudio Viewer.
#' @param graph a graph object of class \code{dgr_graph}.
#' @param shortLabel a logical value.  If set to \code{TRUE}, distribution and formula information is suppressed.
#' @param ... arguments that can be passed onto \code{Diagrammer::create_graph()}.
#' @examples
#' # Render an  empty graph
#' dag_create() %>%
#'   dag_render()
#' @importFrom dplyr select rename mutate filter left_join
#' @importFrom dplyr case_when as_tibble as_data_frame
#' @importFrom DiagrammeR render_graph
#' @export
dag_render <- function(graph,
                       shortLabel = FALSE,
                       wrapWidth = 24,
                       ...) {
  sLabel = shortLabel
  ww = wrapWidth
  graph %>%
    dag_diagrammer(shortLabel = sLabel, wrapWidth = ww) %>%
    DiagrammeR::render_graph(...)
}
