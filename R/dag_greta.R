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
#' @importFrom igraph layout_
#' @export
dag_greta <- function(graph,
                      mcmc = FALSE) {
  ###Extract relevant graph objects
  nodesDF = graph$nodes_df
  edgesDF = graph$edges_df
  plateDF = graph$plate_index_df
  plateNodesDF = graph$plate_nodes_df

  ###Add dimensionality due to plate notation
  ###Retrieve nodes with dimensions
  dimNodesDF = graph$plate_nodes_df %>%
    dplyr::left_join(graph$plate_index_df, by = "indexID") %>%
    dplyr::as_tibble() %>%
    dplyr::left_join(graph$nodes_df, by = c("nodeID" = "id")) %>%
    dplyr::select(nodeID,indexLabel,label,abbrevLabel,type,formulaString, distr, fullDistLabel) %>%
    dplyr::filter(type == "latent" & is.na(formulaString)) #%>%
    #dplyr::select(nodeID, indexLabel)
  ###Add dim argument to distr
 # dimNodesDF %>% dplyr::left_joingraph$nodes_df


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

  ### Add in Sugiyama Layer information to nodesDF (assume more than one node)
  if(nrow(nodesDF) > 1) {
  sugiYamaLayout = graph %>%
    DiagrammeR::to_igraph() %>%
    igraph::layout_(igraph::with_sugiyama())
  nodeRankSugi = sugiYamaLayout$layout[, 2]
  ### use sugiyama to arrange nodes by vertical rank
  nodesDF = nodesDF %>%
    dplyr::mutate(nodeRank = nodeRankSugi) %>%
    dplyr::arrange(desc(nodeRank)) }

  ###Use DAPROPLIMOPO(DAta,PRior,OPeration,LIkelihood,MOdel,POsterior)
  ###Find all nodes that require data based on user input
  ###Err on the side of including a node

  ###DATA:  Create Code for Data Lines
  lhsNodesDF = nodesDF %>%
    dplyr::filter(type == "obs" | data != "as.character(NA)") %>%
    dplyr::filter(!(label %in% plateDF$dataNode)) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(abbrevLabel),
                             " <- ",
                             "as_data(",
                             data,
                             ")")) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #DATA"))

  ###Aggregate Code Statements for DATA
  dataStatements = paste(lhsNodesDF$codeLine,
                         sep = "\n")

  ###DIM:  Create code for getting Dimensions of prior lines
  ###get all nodes that are part of a plate with dataNode
  plateNodesWithData = plateDF %>%
    dplyr::filter(!is.na(dataNode)) %>%
    dplyr::left_join(plateNodesDF, by = c("indexID"))
  ###add dimension of dataNodes
  uniqDataNodes = unique(plateNodesWithData$dataNode)
  dimDF = data.frame(label = uniqDataNodes, stringsAsFactors = FALSE) %>%
    dplyr::left_join(nodesDF) %>%
    dplyr::select(label, data)
  ###make labels for dim variables = to label_dim
  dimStatements = paste(paste0(abbrevLabelPad(paste0(dimDF$label,
                                                     "_dim")),
                               " <- ",
                               "length(",
                               dimDF$data,
                               ")   #DIM"),
                        sep = "\n")

  ###Insert dim argument into fullDistLabel of appropriate nodes
  dimArgDF = plateNodesWithData %>%
    dplyr::left_join(dimDF, by = c("dataNode" = "label")) %>%
    dplyr::mutate(dimLabel = paste0("dim=",dataNode,"_dim"))

  ###Remove last right parenthesis of all affected labels
  nodesDF$fullDistLabel[dimArgDF$nodeID] =
    substr(nodesDF$fullDistLabel[dimArgDF$nodeID],
           1,
           nchar(nodesDF$fullDistLabel[dimArgDF$nodeID])-1)
  ###Add in dim statement and add back last parenthesis
  nodesDF$fullDistLabel[dimArgDF$nodeID] =
    paste0(nodesDF$fullDistLabel[dimArgDF$nodeID],",",
           dimArgDF$dimLabel,")")

  ###PRIOR:  Create code for prior lines
  ###create dataframe of dataNodes and their data
  lhsNodesDF = nodesDF %>%
    dplyr::filter(type == "latent" & is.na(formulaString)) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(abbrevLabel),
                             " <- ",
                             fullDistLabel)) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #PRIOR"))

  ###Aggregate Code Statements for PRIOR
  priorStatements = paste(lhsNodesDF$codeLine,
                          sep = "\n")

  ###OPERATION:  Create code for OPERATION lines
  lhsNodesDF = nodesDF %>%
    dplyr::filter(type == "latent" & !is.na(formulaString)) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(abbrevLabel),
                             " <- ",
                             formulaString)) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #OPERATION"))

  ###Aggregate Code Statements for OPERATION
  opStatements = paste(lhsNodesDF$codeLine,
                       sep = "\n")

  ###LIKELIHOOD:  Create code for LIKELIHOOD lines
  lhsNodesDF = nodesDF %>%
    dplyr::filter(type == "obs") %>%  ##only observed nodes
    dplyr::inner_join(edgesDF, by = c("id" = "to")) %>% # only nodes with parents
    dplyr::distinct(abbrevLabel,fullDistLabel) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(
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

  ###Create MODEL Statement
  # get all non-observed / non-formula nodes by default
  unobservedNodes = graph$nodes_df %>%
    dplyr::filter(type != "obs" & is.na(formulaString)) %>%
    dplyr::pull(label)
  modelStatement = paste0("gretaModel <- model(",
                          paste0(unobservedNodes, collapse = ","),
                          ")   #MODEL")

  ###Create POSTERIOR draws statement
  posteriorStatement = paste0("draws   <- mcmc(gretaModel)   #POSTERIOR\ndrawsDF <- draws %>% as.matrix() %>% as_tibble()   #POSTERIOR\n")

  ##########################################
  ###Aggregate all code
  codeStatements = c(dataStatements,
                     dimStatements,
                     priorStatements,
                     opStatements,
                     likeStatements,
                     modelStatement,
                     posteriorStatement)

  #codeStatements

  ###gretaCode as text
  paste0("## The specified DAG corresponds to the following greta code: \n",
         paste(codeStatements, collapse = '\n')) %>% cat()

  ##EVALUATE CODE IN GLOBAL ENVIRONMENT
  ##make expression out of Code Statements
  codeExpr = parse(text = codeStatements)

  ##eval expression
  if(mcmc == TRUE) {eval(codeExpr, envir = globalenv())}

  ###return code
  return(invisible())
}
