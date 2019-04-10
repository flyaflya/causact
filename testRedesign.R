#node test
dag_node() %>% dag_diagrammer() %>%
  DiagrammeR::render_graph()
dag_create() %>% dag_node() %>% dag_diagrammer() %>%
  DiagrammeR::render_graph()

dag_create() %>% dag_node("test", rhs = normal) %>% dag_diagrammer() %>%
  DiagrammeR::render_graph()

dag_create() %>% dag_node(data = attitude$complaints) %>% dag_diagrammer() %>%
  DiagrammeR::render_graph()

dag_create() %>% dag_node(c("Momma","Poppa"),data = attitude$complaints, rhs = normal(0,10)) %>% dag_diagrammer() %>%
  DiagrammeR::render_graph()

dag_create() %>% dag_node(rhs = normal(0,10), data = attitude[, 2:7]) %>% dag_diagrammer() %>%
  DiagrammeR::render_graph()

dag_create() %>%
  dag_node("response") %>%
  dag_node(label = c("a","b"),
           rhs = alpha+beta*x,
           data = attitude[, 2:7]) %>%
  dag_edge(from = c("attitude$raises","complaints"),
           to = "response") %>%
  dag_diagrammer() %>%
  DiagrammeR::render_graph()

dag_create() %>% dag_node(data = attitude[, 2:7]) %>% dag_diagrammer() %>%
  DiagrammeR::render_graph()

dag_create() %>%
  dag_node(c("comp","priv","learn","rais","crit","adv"),
           data = attitude[, 2:7],
           rhs = normal(0,10)) %>%
  dag_render()

dag_create() %>%
  dag_node(data = attitude[,2:7]) %>%
  dag_plate("Predictors","j",nodeLabels = c("prvl","rass")) %>%
  dag_plate(descr = "Other Predictors",label = "k",nodeLabels = c("crtc","advn","rass"))%>%
  dag_render()

dag_create() %>%
  dag_node(data = attitude[,2:7]) %>%
  dag_plate("Predictors","j",nodeLabels = c("rass")) %>%
  dag_plate(descr = "Other Predictors",label = "k",nodeLabels = c("rass"))%>%
  dag_render()

graph = dag_create() %>%
  dag_node(label = "y",descr = "Sales Price",
           rhs = normal,
           data = houseDF$SalePrice) %>%
  dag_node(label = "mean",descr = "Exp Sales Price",
           child = "Sales Price",
           rhs = alpha + beta * x) %>%
  dag_node(label = "x",descr = "Square Footage",
           data = houseDF$`1stFlrSF`,
           child = "mean") %>%
  dag_node(label = "beta", descr = "Price per Sq Foot",
           child = "mean", rhs = normal(0,10000)) %>%
  dag_node(label = "sd", descr = "Std Dev of Sales Price",
           child = "Sales Price", rhs = lognormal(0,100)) %>%
  dag_node(label = "alpha", descr = "Exp Sales Price",
           child = "mean", rhs = normal(160000,10000)) %>%
  dag_plate("Observation","i",
            nodeLabels = c("y","x","mean")) %>%
  dag_plate(descr = "Zip Code",label = "zip",
            nodeLabels = "alpha",
            data = houseDF$Neighborhood,
            addDataNode = TRUE)
graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_greta()
graph %>% dag_greta(mcmc = TRUE)
tidyDrawsDF %>% dagp_plot()

graph = dag_create() %>%
  dag_node(data = attitude$rating,
           rhs = normal(mu,sd)) %>%
  dag_node("mu", child = "rtng") %>%
  dag_node(data = attitude$complaints, child = "mu") %>%
  dag_node("sd", child = "rating") %>%
  dag_plate("Observations","i",nodeLabels = c("rating","mu","cmpl"))
graph %>% dag_render()

schools_dat <- data.frame(y = c(28,  8, -3,  7, -1,  1, 18, 12),
                          sigma = c(15, 10, 16, 11,  9, 11, 10, 18), schoolName = paste0("School",1:8))

graph = dag_create() %>%
  dag_node("Treatment Effect","y",
           rhs = normal(theta, sigma),
           data = schools_dat$y) %>%
  dag_node("Std Error of Effect Estimates","sigma",
           data = schools_dat$sigma,
           child = "y") %>%
  dag_node("Exp. Treatment Effect","theta",
           child = "y",
           rhs = avgEffect + schoolEffect) %>%
  dag_node("Pop Treatment Effect","avgEffect",
           child = "theta",
           rhs = normal(0,30)) %>%
  dag_node("School Level Effects","schoolEffect",
           rhs = normal(0,30),
           child = "theta") %>%
  dag_plate("Observation","i",nodeLabels = c("sigma","y","theta")) %>%
  dag_plate("School Name","school",
            nodeLabels = "schoolEffect",
            data = schools_dat$schoolName,
            addDataNode = TRUE)
graph %>% dag_render()
graph %>% dag_greta(mcmc=TRUE)
tidyDrawsDF %>% dagp_plot()


graph = dag_create() %>%
  dag_node("Get Card","y",
           rhs = bernoulli(theta),
           data = carModelDF$getCard) %>%
  dag_node(descr = "Card Probability by Car",label = "theta",
           rhs = beta(2,2),
           child = "y") %>%
  dag_node("Car Model","x",
           data = carModelDF$carModel,
           child = "y") %>%
  dag_plate("Car Model","x",
            data = carModelDF$carModel,
            nodeLabels = "theta")

graph %>% dag_render()
graph %>% dag_greta()
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_greta(mcmc=TRUE)
tidyDrawsDF %>% dagp_plot()



graph = dag_create() %>%
  dag_node("Get Card","y",
           rhs = bernoulli(theta),
           data = carModelDF$getCard) %>%
  dag_node("Probability", "theta",
           rhs = beta(2, 2),
           child = "y")
graph %>% dag_dim()
graph %>% dag_render()
graph %>% dag_greta(mcmc=TRUE)  ##takes 1 minute
## with tf 1.12.0 and tf 0.5.0

## should take under 20 seconds with right configuration
drawsDF %>% dagp_plot()
tidyDrawsDF %>% dagp_plot()




descr = "X"
label = as.character(NA)
data = NULL # vector or df
rhs = NULL ##not vectorized
child = as.character(NA) ##not vectorized
obs = FALSE
