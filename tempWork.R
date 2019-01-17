
library(magrittr)

dag_create() %>%
  dag_node("Sales Price","Y","obs",greta::normal) %>%
  dag_render()

temp = dag_create() %>%
  dag_node("Sales Price","Y","obs",greta::normal)

childLabel = "Y"
parentArgName = "mean"
description = "Exp. Sales Price"

####
### get node ID from label
nodeID = temp$nodes_df$id[which(temp$nodes_df$label == childLabel)]

### give name to new node
newNodeLabel = paste0(parentArgName,"_",childLabel)

#### get number of parents
#distribution = temp$nodes_df$distribution[nodeID]
#distString = paste0("greta::",distribution)
#distExpr = parse(text = distString)
#distArgs = formalArgs(eval(distExpr))
###assume arguments before dim are required parameters
#numParents = which(distArgs == "dim") - 1

### create one parent node for each argument
# get number of arguments
#distribution = tail(as.character(substitute(distribution)), n=1)

### create node with edge to child
temp %>% dag_node(graph = temp,description = description,label = newNodeLabel) %>% dag_edge(from = newNodeLabel, to = childLabel) %>% dag_render()
