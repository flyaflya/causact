test_that("dissest df$col notation", {
  renderGraph = ### greta example #3
    graph = dag_create() %>%
    dag_node("Rating","y",
             data = attitude$rating,
             rhs = normal(mu,sd)) %>%
    dag_node("Exp. Rating","mu",
             rhs = int + design %*% coefs,
             child = "y") %>%
    dag_node("Intercept","int",
             rhs = normal(0,10),
             child = "mu") %>%
    dag_node("Std Dev of Ratings","sd",
             rhs = cauchy(0,3, truncation = c(0,Inf)),
             child = "y") %>%
    dag_node("Predictor Coefficients","coefs",
             rhs = normal(mean = 0,sd = 10),
             child = "mu",
             extract = FALSE) %>%
    dag_node("Predictors","design",
             data = attitude[, 2:7],
             keepAsDF = TRUE,
             child = "mu") %>%
    dag_plate(descr = "Predictors",label = "j",
              nodeLabels = c("coefs"),
              data = names(attitude[, 2:7])) %>%
    dag_plate(descr = "Observation", label = "i",
              nodeLabels = c("mu","y"))
  expect_type(renderGraph %>% dag_render(), "list")
})
