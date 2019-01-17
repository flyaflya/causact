 nodeLabels = c(
   "Response \n X\U1D62 \U223C Normal(&mu;\U2C7C,&sigma;)",
   "Response Std. Dev \n &sigma; \U223C Cauchy\U207A(0,1)",
   "Condition ID \n j\U1D62",
   "Condition Mean \n &mu;\U2C7C \U223C Normal(&mu;\U2092,&sigma;\U2092)",
   "Overall Mean \n &mu;\U2092 \U223C Normal(0,5)",
   "CondMean Std. Dev \n &sigma;\U2092 \U223C Cauchy\U207A(0,1)")
 periphLabels = c(1)
 cluster = c("Observation i",NA,"Observation i","Condition j",NA,NA)
 nodeTypes = c("obs","latent","obs","latent","latent","latent","latent")
 nodeDF = create_node_df(n = length(nodeLabels), label = nodeLabels, peripheries = periphLabes, type = nodeTypes, cluster = cluster) # create a one node dataframe
 edgeDF = create_edge_df(from = c(2,3,4,5,6),to = c(1,1,1,4,4))
 create_dag(nodes_df = nodeDF, edges_df = edgeDF) %>% render_dag()

dag_create() %>%
  dag_node(label = "X", type = "latent", description = "Condition") %>%
  dag_node(label = "Y", type = "obs", description = "Response", distribution = greta::normal) %>%
  dag_node(label = "Z", description = "Parent") %>%
  dag_edge(from = "X", to = "Y") %>%
  dag_plate(indexLabel = "i", description = "Observation", nodeLabels = c("X","Z")) %>%
  dag_plate(indexLabel = "j", description = "Condition", nodeLabels = c("Y","Z")) %>%
  dag_edge(from = "Z", to = "Y") %>%
  dag_render()

temp$plate_nodes_df

which(formalArgs(greta::normal)=="dim")

library(greta)
library(DiagrammeR)
library(causact)

### create a causact model
dag_create() %>%
  dag_node(description = "Sales Price",
           label = "Y",
           type = "obs",
           distribution = greta::normal) %>%
  dag_prior(childLabel = "Y",
            parentArgName = "mean",
            description = "Exp. Sales Price",
            formulaString = "alpha + beta * x_i") %>%
  dag_prior(childLabel = "Y",parentArgName = "sd",description = "SD of Sales Price") %>%
  dag_prior(childLabel = "mean_Y", parentArgName = "alpha", description = "Intercept") %>%
  dag_plate(indexLabel = "i",description = "Observation",nodeLabels = c("mean_Y","Y")) %>%
  dag_render()

dag_create() %>%
  dag_node("Sales Price","Y","obs",greta::normal) %>%
  dag_node("Exp. Sales Price", "mean_Y", "latent") %>%
  dag_edge("mean_Y","Y") %>%
  dag_plate(indexLabel = "i",description = "Observation",nodeLabels = c("Y","mean_Y")) %>%
  dag_render()


dag_create() %>%
  dag_node("A","Aidan") %>%
  dag_render()

dag_create() %>%
  dag_node("A","Aidan",distr = greta::beta) %>%
  dag_prior(parentArgName = "AJ",description = "Adam",childLabel = "A", distr = greta::normal) %>%
  dag_prior("E","Erin","A") %>%
  dag_render()


dag_create() %>%
  dag_node("A","Aidan", formulaString = "x + y") %>%
  dag_node("AJ","Adam", children = "A") %>%
  dag_node("E","Erin") %>%
  dag_edge("Erin","A") %>%
#  dag_edge(from = "AJ", to = children) %>%
  dag_render(shortLabel = FALSE)

dag_create() %>%
  dag_node("Square Footage") %>%
  dag_render(shortLabel = TRUE)

