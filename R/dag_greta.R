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
                      mcmc = FALSE,
                      meaningfulLabels = TRUE,
                      ...) {

  ###get dimension information
  graph = graph %>% dag_dim()

  ###update rhs information for labelling computer code
  graph = rhsPriorComposition(graph)
  graph = rhsOperationComposition(graph)

  ###retrieve nodeDF,edgeDF,argDF,plateIndexDF, and plateNodeDF
  nodeDF = graph$nodes_df
  edgeDF = graph$edges_df
  argDF = graph$arg_df
  plateDF = graph$plate_index_df
  plateNodeDF = graph$plate_node_df
  dimDF = graph$dim_df

  ###arrangeNodes in topological order -> top-down
  nodeIDOrder = igraph::graph_from_data_frame(edgeDF %>% select(from,to)) %>%
    igraph::topo_sort(mode = "out") %>%
    names() %>%
    as.integer()

  ## append non-connected nodes into nodeIDOrder
  nodeIDOrder = union(nodeIDOrder,nodeDF$id)

  ## arrange nodeDF by nodeIDOrder
  nodeDF = nodeDF[match(nodeIDOrder,nodeDF$id) , ]

  ###Use DAPROPLIMOPO(DAta,PRior,OPeration,LIkelihood,MOdel,POsterior)
  ###Find all nodes that require data based on user input
  ###Err on the side of including a node

  ### Initialize all the code statements so that NULL
  ### values are skipped without Error
  dataStatements = NULL
  plateDataStatements = NULL
  dimStatements = NULL
  priorStatements = NULL
  opStatements = NULL
  likeStatements = NULL
  modelStatement = NULL
  posteriorStatement = NULL


  ###DATA:  Create Code for Data Lines
  lhsNodesDF = nodeDF %>%
    dplyr::filter(obs == TRUE | !is.na(data)) %>%
    dplyr::filter(!(label %in% plateDF$indexLabel)) %>%
    dplyr::mutate(codeLine = paste0(auto_label,
                             " <- ",
                             "as_data(",
                             data,
                             ")")) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #DATA"))

  ###Aggregate Code Statements for DATA
  if(nrow(lhsNodesDF) > 0) {
    dataStatements = paste(lhsNodesDF$codeLine,
                           sep = "\n")
  }

  ###DIM:  Create code for plate dimensions
    plateDimDF = plateDF %>% dplyr::filter(!is.na(dataNode))
    if (nrow(plateDimDF) > 0) {
      plateDataStatements = paste(paste0(
        abbrevLabelPad(paste0(plateDimDF$indexLabel,
                              "    ")),# four spaces to have invis _dim
                              " <- ",
                              "as.factor(",
                              plateDimDF$dataNode,
                              ")   #DIM"
                                         ),
                              sep = "\n")
    ###make labels for dim variables = to label_dim
      dimStatements = paste(
        paste0(abbrevLabelPad(paste0(plateDimDF$indexLabel,
                                "_dim")),
               " <- ",
               "length(unique(",
               plateDimDF$indexLabel,
               "))   #DIM"
               ),
        sep = "\n"
      )
      }


  ###PRIOR:  Create code for prior lines
  ###create dataframe of dataNodes and their data
  lhsNodesDF = nodeDF %>%
    dplyr::filter(distr == TRUE & obs == FALSE) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(auto_label),
                             " <- ",
                             auto_rhs)) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #PRIOR"))

  ###Aggregate Code Statements for PRIOR
  priorStatements = paste(lhsNodesDF$codeLine,
                          sep = "\n")

  ###OPERATION:  Create code for OPERATION lines
  lhsNodesDF = nodeDF %>%
    dplyr::filter(!is.na(rhs) & distr == FALSE) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(auto_label),
                             " <- ",
                             auto_rhs)) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #OPERATION"))

  ###Aggregate Code Statements for OPERATION
  opStatements = paste(lhsNodesDF$codeLine,
                       sep = "\n")

  ###LIKELIHOOD:  Create code for LIKELIHOOD lines
  lhsNodesDF = nodeDF %>%
    dplyr::filter(obs == TRUE) %>%  ##only observed nodes
    dplyr::inner_join(edgeDF, by = c("id" = "to")) %>% # only nodes with parents
    dplyr::distinct(id,auto_label,auto_rhs) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(
      paste0("distribution(",
             auto_label,
             ")")
    ),
    " <- ",
    auto_rhs)) %>%
    dplyr::mutate(codeLine = paste0(abbrevLabelPad(codeLine), "   #LIKELIHOOD"))

  ###Aggregate Code Statements for LIKELIHOOD
  likeStatements = paste(lhsNodesDF$codeLine,
                         sep = "\n")

  ###Create MODEL Statement
  # get all non-observed / non-formula nodes by default
  unobservedNodes = graph$nodes_df %>%
    dplyr::filter(obs == FALSE & distr == TRUE) %>%
    dplyr::pull(auto_label)

  #group unobserved nodes by their rhs for later plotting by ggplot
  #all nodes sharing the same prior will be graphed on the same scale
  # this code should be moved out of dag_greta at some point
  priorGroupDF = graph$nodes_df %>%
    dplyr::filter(obs == FALSE & distr == TRUE) %>%
    dplyr::mutate(., priorGroup = group_indices(., auto_rhs))
  assign("priorGroupDF", priorGroupDF, envir = cacheEnv)

  modelStatement = paste0("gretaModel <- model(",
                          paste0(unobservedNodes, collapse = ","),
                          ")   #MODEL")

  ###Create POSTERIOR draws statement
  meaningfulLabels(graph,plateDataStatements)  ###assign meaningful labels in cacheEnv
  extraArgList = list(...)
  extraArgString = paste0(paste0(names(extraArgList)," = ", as.character(extraArgList)), collapse = ",")
  mcmcArgs = ifelse(extraArgString == " = ","gretaModel",paste("gretaModel",extraArgString, sep = ","))
  posteriorStatement = paste0("draws       <- mcmc(",mcmcArgs,")   #POSTERIOR\ndraws       <- replaceLabels(draws)   #POSTERIOR\ndrawsDF     <- draws %>% as.matrix() %>% dplyr::as_tibble()   #POSTERIOR\ntidyDrawsDF <- drawsDF %>% tidyr::gather() %>%
    addPriorGroups()   #POSTERIOR\n")

  ##########################################
  ###Aggregate all code
  codeStatements = c(dataStatements,
                     plateDataStatements,
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
