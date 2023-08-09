test_that("gamma Dist in Use", {
  graph = dag_create() %>%
    dag_node("Tree Height","x",
             rhs = student(nu,mu,sigma),
             data = trees$Height) %>%
    dag_node("Degrees Of Freedom","nu",
             rhs = gamma(2,0.1),
             child = "x") %>%
    dag_node("Avg Cherry Tree Height","mu",
             rhs = normal(50,24.5),
             child = "x") %>%
    dag_node("StdDev of Observed Height","sigma",
             rhs = uniform(0,50),
             child = "x") %>%
    dag_plate("Observation","i",
              nodeLabels = "x")
  mcmcCode = graph %>% dag_numpyro(mcmc = FALSE)
  expect_type(mcmcCode, "character")
})

