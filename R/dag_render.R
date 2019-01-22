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
                       ...) {
  ## if graph has zero nodes, return a simple graph that says
  ## start building your model
  if (nrow(graph$nodes_df) == 0) {
    graph = graph %>% dag_node("START MODELLING", distr = "use dag_node()")
  }

  ## rename label for use in diagram
  relation = dplyr::case_when(!is.na(graph$nodes_df$formulaString) ~ " = ",
                              TRUE ~ " ~ ") # equal or tilde

  ## short label implements automatic word wrap and
  ## if descLabel = TRUE, then adds description
  if (shortLabel) {
    graph$nodes_df$label = sapply(ifelse(
      is.na(graph$nodes_df$description),
      ##true
      strwrap(
        graph$nodes_df$label,
        width = 12,
        simplify = FALSE
      ),
      ##FALSE
      strwrap(
        paste0(
          graph$nodes_df$description,
          " (",
          graph$nodes_df$label,
          ")"
        ),
        width = 12,
        simplify = FALSE
      )
    ),
    paste,
    collapse = "\n")
  } else {
    ##long label changes depending on whether greta distribution is used
    ##right now, the test for a greta distribution is the expression in
    ##the below if statement.

    for (i in seq_along(graph$nodes_df$distr)) {
      if (graph$nodes_df$distr[i] == graph$nodes_df$fullDistLabel[i]) {
        if (is.na(graph$nodes_df$description[i])) {
          graph$nodes_df$description[i] = ""
        }  ## stop NA from printing
        graph$nodes_df$label[i] = paste0(
          graph$nodes_df$description[i],
          "\n",
          graph$nodes_df$label[i],
          " ",
          graph$nodes_df$fullDistLabel[i]
        )
      } else {
        if (is.na(graph$nodes_df$description[i])) {
          graph$nodes_df$description[i] = ""
        }  ## stop NA from printing
        graph$nodes_df$label[i] = sapply(strwrap(
          paste0(
            graph$nodes_df$description[i],
            " (",
            graph$nodes_df$label[i],
            ")",
            ifelse(is.na(graph$nodes_df$formulaString[i]), " ~ ", " = "),
            graph$nodes_df$fullDistLabel[i]
          ),
          width = 25,
          simplify = FALSE
        ),
        paste,
        collapse = "\n")
      }  #end else
    }  #end for
  }  #end else

  graph %>% render_graph()  ### render the graph
}
