#' Create a graph object focused on drawing a DAG.
#'
#' The graph object should be of class \code{dgr_graph} and created using \code{dag_create()}.
#' @param graph a graph object of class \code{dgr_graph} created using \code{dag_create()} with data argument populated.
#' @examples
#' # With `dag_create()` we can
#' # simply create an empty graph (and
#' # add in nodes and edges later
#' # with other functions)
#' dag_create() %>% dag_render(shortLabel = TRUE)
#' @importFrom dplyr bind_rows tibble
#' @importFrom DiagrammeR create_graph add_global_graph_attrs
#' @importFrom rlang enquo expr_text
#' @export
dag_greta <- function(graph) {
  ###Extract relevant graph objects
  nodesDF = graph$nodes_df
  edgesDF = graph$edges_df
  plateDF = graph$plate_index_df
  plateNodesDF = graph$plate_nodes_df


  ### Substitute any abbreviated labels within parantheses of fullDistLabel
  ### or within formula.  Only check nodes with edges
  for (i in nodesDF$label) {
    ## i is label of parent nodes
    ##check if label is abbreviated
    if (i == nodesDF$abbrevLabel[which(nodesDF$label == i)]) {
      next
    }

    ##if label is abbreviated get children nodes
    parentNodeIndex = which(nodesDF$label == i)
    parentNodeID = nodesDF$id[parentNodeIndex]
    childNodeIDs = edgesDF$to[which(edgesDF$from == parentNodeID)]

    for (j in childNodeIDs) {
      ## j is child node id
      childNodeIndex = which(nodesDF$id == j)
      ## if child has formula string, replace any instance of
      ## parent label with the abbrevlabel
      if (!is.na(nodesDF$formulaString[childNodeIndex])) {
        ###code to sub out big label for small label
        nodesDF$formulaString[childNodeIndex] =
          gsub(nodesDF$label[parentNodeIndex],
               ##parent full label
               nodesDF$abbrevLabel[parentNodeIndex],
               ##parent abbrev label
               nodesDF$formulaString[childNodeIndex],
               ##replace in formula string
               fixed = TRUE)
      } else  {
        ## abbrev in fullDistLabel
        ###code to sub out big label for small label
        nodesDF$fullDistLabel[childNodeIndex] =
          gsub(nodesDF$label[parentNodeIndex],##parent full label
               nodesDF$abbrevLabel[parentNodeIndex],##parent abbrev label
               nodesDF$fullDistLabel[childNodeIndex], ##replace in formula string
               fixed = TRUE)
      }
    }
  }

  ### Add in Sugiyama Layer information to nodesDF
  sugiYamaLayout = graph %>%
    DiagrammeR::to_igraph() %>%
    layout_(with_sugiyama())
  nodeRankSugi = sugiYamaLayout$layout[, 2]
  ### use sugiyama to arrange nodes by vertical rank
  nodesDF = nodesDF %>%
    dplyr::mutate(nodeRank = nodeRankSugi) %>%
    arrange(desc(nodeRank))

  ###Use DAPROPLIMOPO(DAta,PRior,OPeration,LIkelihood,MOdel,POsterior)
  ###Find all nodes that require data based on user input
  ###Err on the side of including a node

  ###DATA:  Create Code for Data Lines
  lhsNodesDF = nodesDF %>%
    dplyr::filter(type == "obs" | data != "as.character(NA)") %>%
    mutate(codeLine = paste0(abbrevLabelPad(abbrevLabel),
                             " <- ",
                             "as_data(",
                             data,
                             ")")) %>%
    mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #DATA"))

  ###Aggregate Code Statements for DATA
  dataStatements = paste(lhsNodesDF$codeLine,
                         sep = "\n")

  ###PRIOR:  Create code for prior lines
  lhsNodesDF = nodesDF %>%
    dplyr::filter(type == "latent" & is.na(formulaString)) %>%
    mutate(codeLine = paste0(abbrevLabelPad(abbrevLabel),
                             " <- ",
                             fullDistLabel)) %>%
    mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #PRIOR"))

  ###Aggregate Code Statements for PRIOR
  priorStatements = paste(lhsNodesDF$codeLine,
                          sep = "\n")

  ###OPERATION:  Create code for OPERATION lines
  lhsNodesDF = nodesDF %>%
    dplyr::filter(type == "latent" & !is.na(formulaString)) %>%
    mutate(codeLine = paste0(abbrevLabelPad(abbrevLabel),
                             " <- ",
                             formulaString)) %>%
    mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #OPERATION"))

  ###Aggregate Code Statements for OPERATION
  opStatements = paste(lhsNodesDF$codeLine,
                       sep = "\n")

  ###LIKELIHOOD:  Create code for LIKELIHOOD lines
  lhsNodesDF = nodesDF %>%
    dplyr::filter(type == "obs") %>%  ##only observed nodes
    dplyr::inner_join(edgesDF, by = c("id" = "to")) %>% # only nodes with parents
    dplyr::distinct(abbrevLabel,fullDistLabel) %>%
    mutate(codeLine = paste0(abbrevLabelPad(
      paste0("distribution(",
             abbrevLabel,
             ")")
    ),
    " <- ",
    fullDistLabel)) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #LIKELIHOOD"))

  ###Aggregate Code Statements for LIKELIHOOD
  likeStatements = paste(lhsNodesDF$codeLine,
                         sep = "\n")

  ##########################################
  ###Aggregate all code
  codeStatements = c(dataStatements,
                     priorStatements,
                     opStatements,
                     likeStatements)

  #codeStatements

  ###gretaCode as text
  paste("The following code was run in the background: \n",
        codeStatements, collapse = '\n') %>% cat()

  ##EVALUATE CODE IN GLOBAL ENVIRONMENT
  ##make expression out of Code Statements
  codeExpr = parse(text = codeStatements)

  ##eval expression
  eval(codeExpr, envir = globalenv())

  ###return code
  return()
}
