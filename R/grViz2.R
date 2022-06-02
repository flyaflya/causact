#' R + viz.js
#'
#' Make diagrams in R using [viz.js](https://github.com/mdaines/viz.js)
#' with infrastructure provided by
#' [htmlwidgets](http://www.htmlwidgets.org/).
#' @param diagram spec for a diagram as either text, filename string, or file
#'   connection.
#' @param engine string for the Graphviz layout engine; can be `dot`
#'   (default), `neato`, `circo`, or `twopi`. For more
#'   information see
#'   [viz.js usage](https://github.com/mdaines/viz.js).
#' @param allow_subst a boolean that enables/disables substitution
#'   functionality.
#' @param options parameters supplied to the htmlwidgets framework.
#' @param width an optional parameter for specifying the width of the resulting
#'   graphic in pixels.
#' @param height an optional parameter for specifying the height of the
#'   resulting graphic in pixels.
#' @return An object of class `htmlwidget` that will intelligently print
#'   itself into HTML in a variety of contexts including the R console, within
#'   R Markdown documents, and within Shiny output bindings.
#' @importFrom rstudioapi isAvailable
#' @importFrom htmlwidgets sizingPolicy

grViz2 <- function(diagram = "",
                  engine = "dot",
                  allow_subst = TRUE,
                  options = NULL,
                  width = NULL,
                  height = NULL) {

  # Check for a connection or file
  if (inherits(diagram, "connection") ||
      file.exists(diagram)) {

    diagram <-
      readLines(diagram, encoding = "UTF-8", warn = FALSE)

    diagram <- paste0(diagram, collapse = "\n")

  } else {

    # Concatenate any vector with length > 1
    if (length(diagram) > 1) {
      diagram <- paste0(diagram, collapse = "\n")
    }
  }

  if (allow_subst == TRUE) {
    diagram <- replace_in_spec2(diagram)
  }

  # Single quotes within a diagram spec are problematic
  # so try to replace with `\"`
  diagram <- gsub(x = diagram, "'", "\"")

  # Forward options using `x`
  x <-
    list(
      diagram = diagram,
      config = list(
        engine = engine,
        options = options))

  # Only use the Viewer for newer versions of RStudio,
  # but allow other, non-RStudio viewers
  viewer.suppress <-
    rstudioapi::isAvailable() &&
    !rstudioapi::isAvailable("0.99.120")

  # Create the widget
  htmlwidgets::createWidget(
    name = "grViz",
    x = x,
    width = width,
    height = height,
    package = "DiagrammeR",
    htmlwidgets::sizingPolicy(viewer.suppress = viewer.suppress))
}
