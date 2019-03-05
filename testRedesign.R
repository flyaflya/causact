#node test
dag_node()
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

dag_create() %>% dag_node("response") %>% dag_node(label = c("a","b"),rhs = alpha+beta*x, data = attitude[, 2:7]) %>% dag_edge(from = c("attitude$raises","complaints"), to = "response") %>% dag_diagrammer() %>%
  DiagrammeR::render_graph()

dag_create() %>% dag_node(data = attitude[, 2:7]) %>% dag_diagrammer() %>%
  DiagrammeR::render_graph()

dag_create() %>%
  dag_node(c("comp","priv","learn","rais","crit","adv"),
           data = attitude[, 2:7],
           rhs = normal(0,10))

dag_create() %>%
  dag_node("Sales Price","Y",
           rhs = normal(mean,SD),
           obs = TRUE,
           data = attitude$rating)

dag_create() %>%
  dag_node(label = "y",descr = "Sales Price",
           rhs = normal(mean,SD),
           obs = TRUE,
           data = attitude$rating) %>%
  dag_node(label = "x",descr = "Square Footage",
           obs = TRUE,
           data = attitude$complaints) %>%
  dag_node(label = "mean",descr = "Exp Sales Price",
           child = "Sales Price",
           rhs = alpha + beta * x) %>%
  dag_node(label = "zip",descr = "Zip Code",child = "mean", data = attitude$raises) %>%
  dag_node(label = "beta", descr = "Price per Sq Foot",
           child = "mean", rhs = normal(0,10000)) %>%
  dag_node(label = "SD", descr = "Std Dev of Sales Price",
           child = "Sales Price", rhs = lognormal(0,100)) %>%
  dag_node(label = "alpha", descr = "Intercept",
           child = "mean", rhs = normal(160000,10000))
descr = "X"
label = as.character(NA)
data = NULL # vector or df
rhs = NULL ##not vectorized
child = as.character(NA) ##not vectorized
obs = FALSE
