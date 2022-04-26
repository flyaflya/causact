test_that("data frame is created", {
  listObj = dag_create()
  expect_type(listObj, "list")
})

test_that("nodes are created", {
  listObj = dag_create() %>%
    dag_node("Child Node","y") %>%
    dag_node("Parent Node","x")
  expect_type(listObj, "list")
  expect_equal(listObj$nodes_df$descr[1], "Child Node")
  expect_equal(listObj$nodes_df$descr[2], "Parent Node")
})

test_that("edge is created", {
  listObj1 = dag_create() %>%
    dag_node("Child Node","y") %>%
    dag_node("Parent Node","x", child = "y")
  listObj2 = dag_create() %>%
    dag_node("Child Node","y") %>%
    dag_node("Parent Node","x") %>%
    dag_edge("x","y")
  expect_equal(listObj1$edges_df$from, 2)
  expect_equal(listObj2$edges_df$from, 2)
  expect_equal(listObj1$edges_df$to, 1)
  expect_equal(listObj2$edges_df$to, 1)
})

test_that("plate is created", {
  graph = dag_create() %>%
    dag_node("Child Node","y") %>%
    dag_node("Parent Node","x") %>%
    dag_edge("x","y") %>%
    dag_plate("Observation","i",
              nodeLabels = "y")
  expect_type(graph, "list")
  expect_equal(graph$nodes_df$descr[1], "Child Node")
  expect_equal(graph$nodes_df$descr[2], "Parent Node")
  expect_equal(graph$edges_df$from, 2)
  expect_equal(graph$edges_df$from, 2)
  expect_equal(graph$edges_df$to, 1)
  expect_equal(graph$edges_df$to, 1)
  expect_equal(graph$plate_index_df$indexDescription, "Observation")
  expect_equal(graph$plate_node_df$nodeID, 1)
})

test_that("graph can be rendered", {
  renderGraph = dag_create() %>%
    dag_node("# of Hot Day Beachgoers") %>%
    dag_node(c("Rent by Location",
               "Beachgoer Location Probability"),
             obs = TRUE) %>%
    dag_node(c("Revenue","Expenses","Profit"),
             det = TRUE) %>%
    dag_node("Bathhouse Location",
             dec = TRUE) %>%
    dag_edge(from = c("# of Hot Day Beachgoers",
                      "Beachgoer Location Probability",
                      "Bathhouse Location"),
             to = "Revenue") %>%
    dag_edge(from = c("Bathhouse Location",
                      "Rent by Location"),
             to = "Expenses") %>%
    dag_edge(from = c("Revenue","Expenses"),
             to = "Profit") %>%
    dag_render()
  expect_type(renderGraph, "list")
})

test_that("diagrammer object can be created", {
  renderGraph = dag_create() %>%
    dag_node("# of Hot Day Beachgoers") %>%
    dag_node(c("Rent by Location",
               "Beachgoer Location Probability"),
             obs = TRUE) %>%
    dag_node(c("Revenue","Expenses","Profit"),
             det = TRUE) %>%
    dag_node("Bathhouse Location",
             dec = TRUE) %>%
    dag_edge(from = c("# of Hot Day Beachgoers",
                      "Beachgoer Location Probability",
                      "Bathhouse Location"),
             to = "Revenue") %>%
    dag_edge(from = c("Bathhouse Location",
                      "Rent by Location"),
             to = "Expenses") %>%
    dag_edge(from = c("Revenue","Expenses"),
             to = "Profit") %>%
    dag_diagrammer()
  expect_type(renderGraph, "list")
})

test_that("greta code is created", {
  graph = dag_create() %>%
    dag_node("Get Card","x",
             rhs = bernoulli(theta),
             data = carModelDF$getCard) %>%
    dag_node("Signup Probability","theta",
             rhs = uniform(0,1),
             child = "x") %>%
    dag_plate("Car Model", "y",
              data = carModelDF$carModel,
              nodeLabels = "theta",
              addDataNode = TRUE)  %>%
    dag_plate("Observations", "i",
              nodeLabels = c("x","y"))
  gretaCode = graph %>% dag_greta(mcmc = FALSE)
  expect_type(gretaCode, "character")
})

test_that("dag plot creates graph", {
  df = data.frame(x = c(1:5),y = c(6:10))
  plotGr = df %>% dagp_plot()
  expect_type(plotGr, "list")
})


test_that("dag merge can combine graphs", {
  g1 = dag_create() %>%
    dag_node("Demand for A","dA",
             rhs = normal(15,4)) %>%
    dag_node("Supply for A","sA",
             rhs = uniform(0,100)) %>%
    dag_node("Profit for A","pA",
             rhs = min(sA,dA)) %>%
    dag_edge(from = c("dA","sA"),to = c("pA"))
  g2 <- dag_create() %>%
    dag_node("Demand for B","dB",
             rhs = normal(20,8)) %>%
    dag_node("Supply for B","sB",
             rhs = uniform(0,100)) %>%
    dag_node("Profit for B","pB",
             rhs = min(sB,dB)) %>%
    dag_edge(from = c("dB","sB"),to = c("pB"))
  newGraph = g1 %>% dag_merge(g2)
  expect_equal(NROW(newGraph), 6)
})

test_that("bernoulli rvs can be generated", {
  expect_equal(sum(rbern(10,1)), 10)
})

test_that("big models are okay", {
  graph2 = dag_create() %>%
    dag_node("Number of Signups","k",
             rhs = binomial(nTrials,theta),
             data = gymDF$nSigned) %>%
    dag_node("Signup Probability","theta",
             child = "k",
             rhs = 1 / (1+exp(-y))) %>%
    dag_node("Number of Trials","nTrials",
             child = "k",
             data = gymDF$nTrialCustomers) %>%
    dag_node("Linear Predictor","y",
             rhs = alpha + beta * x,
             child = "theta") %>%
    dag_node("Yoga Stretch Flag","x",
             data = gymDF$yogaStretch,
             child = "y") %>%
    dag_node("Gym Intercept","alpha",
             rhs = normal(mu_alpha,sd_alpha),
             child = "y") %>%
    dag_node("Gym Yoga Slope Coeff","beta",
             rhs = normal(mu_beta,sd_beta),
             child = "y") %>%
    dag_node("Avg Crossfit Intercept","mu_alpha",
             rhs = normal(-1,1.5),
             child = "alpha") %>%
    dag_node("Avg Crossfit Yoga Slope","mu_beta",
             rhs = normal(0,0.75),
             child = "beta") %>%
    dag_node("SD Crossfit Intercept","sd_alpha",
             rhs = uniform(0,3),
             child = "alpha") %>%
    dag_node("SD Crossfit Yoga Slope","sd_beta",
             rhs = uniform(0,1.5),
             child = "beta") %>%
    dag_plate("Gym","j",
              nodeLabels = c("alpha","beta"),
              data = gymDF$gymID,
              addDataNode = TRUE) %>%
    dag_plate("Observation","i",
              nodeLabels = c("k","x","j",
                             "nTrials","theta","y"))
  expect_equal(NROW(graph2$nodes_df), 12)
  renderGraph = graph2 %>% dag_render(shortLabel = 35, wrapWidth = 22)
  expect_type(renderGraph, "list")
  gretaOut = graph2 %>% dag_greta(mcmc = FALSE, meaningfulLabels = FALSE)
  expect_type(gretaOut, "character")
})
