test_that("numpyro code is created", {
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
  mcmcCode = graph %>% dag_numpyro(mcmc = FALSE)
  expect_type(mcmcCode, "character")
  labelOut = meaningfulLabels(graph)
  expect_type(labelOut, "NULL")
})

