#' Merge two non-intersect \code{causact_graph} objects
#'
#' Generates a single \code{causact_graph} graph object that combines the multiple provided graphs.
#'
#' @param graph1 A causact_graph objects to be merged with
#' @param ...    As many causact_graph's as wish to be merged
#' @return a merged graph object of class \code{causact_graph}.  Useful for creating simple graphs and then merging them into a more complex structure.
#' @examples
#' # With `dag_merge()` we
#' # reset the node ID's and all other item ID's,
#' # bind together the rows of all given graphs, and
#' # add in nodes and edges later
#' # with other functions
#' # to connect the graph.
#' #
#' # THE GRAPHS TO BE MERGED MUST BE DISJOINT
#' # THERE CAN BE NO IDENTICAL NODES OR PLATES
#' # IN EACH GRAPH TO BE MERGED, AT THIS TIME
#'
#'
#' g1 = dag_create() %>%
#'  dag_node("Demand for A","dA",
#'            rhs = normal(15,4)) %>%
#'   dag_node("Supply for A","sA",
#'            rhs = uniform(0,100)) %>%
#'   dag_node("Profit for A","pA",
#'            rhs = min(sA,dA)) %>%
#'   dag_edge(from = c("dA","sA"),to = c("pA"))
#'
#'
#' g2 <- dag_create() %>%
#'   dag_node("Demand for B","dB",
#'            rhs = normal(20,8)) %>%
#'   dag_node("Supply for B","sB",
#'            rhs = uniform(0,100)) %>%
#'   dag_node("Profit for B","pB",
#'            rhs = min(sB,dB)) %>%
#'   dag_edge(from = c("dB","sB"),to = c("pB"))
#'
#' g1 %>% dag_merge(g2) %>%
#'   dag_node("Total Profit", "TP",
#'            rhs = sum(pA,pB)) %>%
#'   dag_edge(from=c("pA","pB"), to=c("TP")) %>%
#'   dag_render()
#' @importFrom dplyr bind_rows
#' @export

dag_merge <- function(graph1,...){

  ## First validate that the every argument is indeed a causact_graph
  for (graph in list(graph1,...)){
    class_g <- class(graph)
    ## Any causact_graph will have class length of 1
    if(length(class_g) > 1){
      ## This specific case is hard-coded as it has occured often in early use by the author
      if(class_g[1] == chr("grViz") && class_g[2]=="htmlwidget"){
        errorMessage <- paste0("Given rendered Causact Graph. Check the declaration for a dag_render() call.")
      }
      else {
        errorMessage <- paste0("Cannot render given object as it is not a Causact Graph.")
      }
      stop(errorMessage)
    }
    ## Now check the single class
    if(class_g != "causact_graph"){
      errorMessage <- paste0("Cannot render given object as it is not a Causact Graph.")
      stop(errorMessage)
    }
  }

  ## We will merge the graphs into the first one
  final <- graph1

  ## For every dataframe that makes up a graph, we bind them with their
  ## corresponding dataframe in 'final'
  for (graph in list(...)){
    ## All df have first column of id's to be offset by the number already in the
    ## final df

    ## Anytime a node ID is updated, we use the length of the node df df=1
    ## Anytime an ID of a general df item is updated, we use the lenght of the corresponding df

    # For nodes_df we need to offset the rhs and dim id's
    graph[['nodes_df']][['rhsID']] <- c(graph[['nodes_df']][['rhsID']] + length(final[['nodes_df']][['rhsID']]))
    graph[['nodes_df']][['dimID']] <- c(graph[['nodes_df']][['dimID']] + length(final[['nodes_df']][['dimID']]))

    # For edges_df we need to offset the from and to id's which refernce nodes
    graph[['edges_df']][['from']] <- c(graph[['edges_df']][['from']] + length(final[['nodes_df']][['id']]))
    graph[['edges_df']][['to']] <- c(graph[['edges_df']][['to']] + length(final[['nodes_df']][['id']]))


    # For plate_node_df and dim_df adjust the nodeID, which reference nodes
    graph[['plate_node_df']][['nodeID']] <- c(graph[['plate_node_df']][['nodeID']] + length(final[['nodes_df']][['id']]))
    graph[['dim_df']][['nodeID']] <- c(graph[['dim_df']][['nodeID']] + length(final[['nodes_df']][['id']]))


    ## Thus, here we update the general item ID's for each df
    ## And bind the rows of each df to the corresponding df in the final graph
    for  (df in seq_along(final)){
      graph[[df]][[1]] <- c(graph[[df]][[1]] + length(final[[df]][[1]]))
      final[[df]] <- as.data.frame(bind_rows(final[[df]],graph[[df]]))
    }
  }

  ## Return the final Graph
  ## Connecting Nodes and Edges must be added after the graph is assembled
  return(final)
}
