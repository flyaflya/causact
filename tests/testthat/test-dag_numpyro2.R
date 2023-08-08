test_that("large numpyro code is created", {
  graph = dag_create() %>%
    dag_node("Rating","y",
             data = attitude$rating,
             rhs = normal(mu,sd)) %>%
    dag_node("Exp. Rating","mu",
             rhs = int + coef * x,
             child = "y") %>%
    dag_node("Average Population Rating", "int",
             rhs = normal(0,10),
             child = "mu") %>%
    dag_node("Rating Change Per Complaint","coef",
             rhs = normal(0,10),
             child = "mu") %>%
    dag_node("Complaints","x",
             data = attitude$complaints,
             child = "mu") %>%
    dag_node("Std Dev of Ratings","sd",
             rhs = cauchy(0, 3, truncation = c(0,Inf)),
             child = "y") %>%
    dag_plate("Observation", "i",
              nodeLabels = c("y","mu","x"))
  mcmcCode = graph %>% dag_numpyro(mcmc = FALSE)
  expect_type(mcmcCode, "character")
})

