### greta example 1
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

graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_greta()
graph %>% dag_greta(mcmc = TRUE)
library(tidyverse)
drawsDF %>% gather() %>%
  ggplot(aes(x = value, y = ..scaled..)) +
  geom_density(aes(fill = key)) +
  facet_wrap(~key, scales = "free_x") + theme_minimal()


### greta example #2
graph = dag_create() %>%
  dag_node("Rating","y",
           data = attitude$rating,
           rhs = normal(mu,sd)) %>%
  dag_node("Exp. Rating","mu",
           rhs = int + design %*% coefs,
           child = "y") %>%
  dag_node("Std Dev of Ratings","sd",
           rhs = cauchy(0,3, truncation = c(0,Inf)),
           child = "y") %>%
  dag_node("Predictor Coefficients","coefs",
           rhs = normal(0,10),
           child = "mu") %>%
  dag_node("Predictors","design",
           data = attitude[, 2:7],
           keepAsDF = TRUE,
           child = "mu") %>%
  dag_plate(descr = "Predictors",label = "j",
            nodeLabels = c("coefs"),
            data = names(attitude[, 2:7]),
            addDataNode = FALSE) %>%
  dag_plate(descr = "Observation", label = "i",
            nodeLabels = c("mu","y"))

graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_greta()
graph %>% dag_greta(mcmc = TRUE)
drawsDF %>% dagp_plot()

