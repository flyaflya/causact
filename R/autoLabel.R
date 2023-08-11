### function that populates auto labels and decriptions for a
### graphical picture of the DAG
autoLabel = function(graph){

  ### start with data ... for now, exclude DF name from label
  ### uses dfColToJustCol function from utils.R
  graph$nodes_df$auto_data = dfColToJustCol(graph$nodes_df$data)

  ### if description is NA, use data value
  graph$nodes_df$auto_descr = ifelse(is.na(graph$nodes_df$descr),
                                graph$nodes_df$auto_data,
                                graph$nodes_df$descr)

  ### if label is NA, use abbreviated description
  graph$nodes_df$auto_label = ifelse(
    is.na(graph$nodes_df$label),
    abbreviate(make_unique_No_periods(
      graph$nodes_df$auto_descr)),
    graph$nodes_df$label)
  ### make unique node names without periods

  return(graph)
}




