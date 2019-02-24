### allow parameters to be added to distr specification
### parameters

library(causact)
houseDF

graph = dag_create() %>%
  dag_node("y","Sales Price",
           distr = normal(mean,SD),
           observed = TRUE,
           data = houseDF$SalePrice) %>%
  dag_node("x","Square Footage",
           observed = TRUE,
           data = houseDF$`1stFlrSF`) %>%
  dag_node("mean","Exp Sales Price",
           children = "Sales Price",
           formulaString = "alpha + beta * x") %>%
  dag_node("zip","Zip Code",children = "mean", data = houseDF$Neighborhood) %>%
  dag_node("beta", "Price per Sq Foot",
           children = "mean", distr = normal(0,10000)) %>%
  dag_node("SD", "Std Dev of Sales Price",
           children = "Sales Price", lognormal(0,100)) %>%
  dag_node("alpha", "Intercept",
           children = "mean", distr = normal(160000,10000))  %>%
  dag_edge("Square Footage","mean") %>%
  dag_plate("i","Observation",
            nodeLabels = c("y","mean","Square Footage","zip")) %>%
  dag_plate("j","Zip Code",
            nodeLabels = c("Intercept","Price per Sq Foot"),
            dataNode = "zip")
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_greta()
graph %>% dag_render(shortLabel = FALSE)

graph = graph %>% dag_node("hyperBeta", children = "beta")
graph %>% dag_render(shortLabel = FALSE)

dag_create() %>%
  dag_node("Tristan") %>%
  dag_node("A1","Adam", children = "Tristan") %>%
  dag_node("Aidan") %>%
  dag_node("Erin") %>%
  dag_edge("Erin","Aidan") %>%
  dag_edge("Erin","Tristan") %>%
  dag_edge("Adam","Aidan") %>%
  dag_render(shortLabel = TRUE)

###simple LR

graph = dag_create() %>%
  dag_node("Y","Sales Price", distr = normal(mean,sd), observed = TRUE, data = houseDF$SalePrice) %>%
  dag_node("x","Square Footage",
           observed = TRUE, data = houseDF$`1stFlrSF`) %>%
  dag_node("mean","Exp Sales Price",
           children = "Sales Price",
           formulaString = "alpha + beta * x") %>%
  dag_node("beta", "Price per Sq Foot",
           children = "mean", distr = normal(0,10000)) %>%
  dag_node("sd", "Std Dev of Sales Price",
           distr = lognormal(0,3),
           children = "Sales Price") %>%
  dag_node("alpha", "Intercept",
           children = "mean", distr = normal(0,10000))  %>%
  dag_edge("Square Footage","mean") %>% dag_render(shortLabel = TRUE)
graph %>% dag_render()

dag_create() %>%
  dag_node("X") %>%
  dag_node("Y") %>%
  dag_edge(from = "X", to = "Y") %>%
  dag_render(shortLabel = TRUE)

dag_create() %>%
  dag_node("x",distr = normal, data = houseDF$SalePrice) %>%
  dag_render(shortLabel = TRUE)

library(causact)
dag_create() %>%
  dag_node("SalePrice",distr = normal(0,2,truncation = c(0,Inf))) %>%
  #dag_render()
  dag_greta()


dag_create() %>%
  dag_node("SalePrice", data = houseDF$SalePrice, distr = normal) %>%
  dag_node("mean","Exp. Sales Price",
           children = "SalePrice", formulaString = "alpha + beta * x") %>%
  dag_node("alpha","intercept",
           children = "mean", distr = normal(180000,10000)) %>%
  dag_node("beta","Price Per Sq Ft",
           children = "mean", distr = normal(0,1000)) %>%
  dag_node("x","Square Footage",
           children = "mean", data = houseDF$`1stFlrSF`) %>%
  dag_node("sd","Std Dev. of Price",
           children = "SalePrice", distr = lognormal(10,10)) %>%
  dag_render(shortLabel = FALSE)
  dag_greta(mcmc = TRUE)

graph = dag_create() %>%
  dag_node("rating",data = attitude$rating, distr = normal(mu, sd)) %>%
  dag_node("mu", formulaString = "int + design %*% coef", child = "rating") %>%
  dag_node("int", children = "mu", distr = normal(0,10)) %>%
  dag_node("design", children = "mu",
           data = as.matrix(attitude[ , 2:7])) %>%
  dag_node("coef", distr = normal(0,10),children = "mu") %>%
  dag_node("predictor", data = as.matrix(attitude[, 2:7])) %>%
  dag_plate("j","Predictor Variables", nodeLabels = c("design","coef"),
            dataNode = "predictor") %>%
  dag_plate("i","Observation", nodeLabels = c("design","mu","rating"))
  graph %>% dag_greta()
  graph %>% dag_render(shortLabel = FALSE)
