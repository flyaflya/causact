#' Add a node to an existing graph object
#'
#' The graph object should be of class \code{dgr_graph} and created using \code{dag_create()}.
#' @param graph a graph object of class \code{dgr_graph} created using \code{dag_create()}.
#' @param label a required character label that describes the node.
#' @param description a longer more descriptive character label for the node.
#' @param distr a variable greta array function such as \code{uniform, normal, lognormal, bernoulli,} etc. Function arguments are optional.  Use \code{distr = NA} to suppress distribution assumptions.  Valid values include \code{greta::normal}, \code{normal}, and \code{normal(6,2)}.
#' @param observed a logical value indicating whether the node is observed.
#' @param formulaString a character string representing an operation that should be performed to calculate the node's value from its parents.  When input, \code{distr} is ignored.
#' @param children an optional character vector of existing node labels or node descriptions.  Directed edges from the newly created node will be created.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph and add 2 nodes by using
#' # the `dag_node()` function twice
#' graph <-
#'   dag_create() %>%
#'   dag_node("X") %>%
#'   dag_node("Y", children = "X")
#' dag_render(graph, shortLabel = TRUE)
#'
#' dag_create() %>%
#'   dag_node("Y","Sales Price",
#'            distr = normal, observed = TRUE) %>%
#'   dag_node("X","Square Footage",
#'            observed = TRUE, distr = NA) %>%
#'   dag_node("mean","Exp Sales Price",
#'            children = "Sales Price",
#'            formulaString = "alpha + beta * x") %>%
#'   dag_node("beta", "Price per Sq Foot",
#'            children = "mean") %>%
#'   dag_node("SD", "Std Dev of Sales Price",
#'            distr = lognormal(0,3),
#'            children = "Sales Price") %>%
#'   dag_node("alpha", "Intercept",
#'            children = "mean")  %>%
#'   dag_edge("Square Footage","mean") %>%
#'   dag_render()
#' @importFrom dplyr select bind_cols as_tibble case_when
#' @importFrom rlang is_empty UQ enexpr
#' @importFrom DiagrammeR add_node node_data node_aes
#' @export
dag_node <- function(graph,
                     label,
                     description = as.character(NA),
                     distr = greta::variable(-Inf,Inf),
                     observed = FALSE,
                     formulaString = as.character(NA),
                     children = as.character(NA)) {

  # update node_data with DAG specific graphing
  description = description
  distr = rlang::enexpr(distr)  ## take in argument as expression
  type = ifelse(observed == TRUE,"obs","latent")
  formulaString = formulaString

  # get node labels based off of user input for distr
  distList = getFullDistList(rlang::UQ(distr))

  ## above returns list of dist name, dist arguments, and label
  distString = distList$distString
  fullDistLabel = distList$fullDistLabel

  ##use formula string for label if available
  ##distribution is ignored when formulaString is provided
  if (!is.na(formulaString)) {
    fullDistLabel = formulaString
  }

  fillcolor = dplyr::case_when(observed == TRUE ~ "cadetblue",
                               TRUE ~ "aliceblue")

  graph = graph %>% DiagrammeR::add_node(
    type = type,
    label = label,
    node_data =
      DiagrammeR::node_data(
        description = description,
        distr = distString,
        fullDistLabel = fullDistLabel,
        formulaString = formulaString),
    node_aes =
      DiagrammeR::node_aes(peripheries = ifelse(is.na(formulaString), 1, 2),
                           fillcolor = fillcolor,
                           fontcolor = "black")
  )

  # Add child edge if desired
  if (!is.na(children)) {
    graph = graph %>% dag_edge(from = label, to = children)
  }


  return(graph)
}
