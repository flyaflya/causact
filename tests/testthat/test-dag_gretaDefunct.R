test_that("dag_greta gives warning", {
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
  mcmcCode = graph %>% dag_greta(mcmc = FALSE)
  expect_type(mcmcCode, "NULL")
})

