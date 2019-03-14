### greta example 0
graph = dag_create() %>%
  dag_node("Sepal Length","y",
           data = iris$Petal.Length,
           rhs = normal) %>%
  dag_node("Exp Sepal Length (Obs.)","mean",
           rhs = int + coef * x,
           child = "y") %>%
  dag_node("Exp Sepal Length (Pop.)","int",
           rhs = normal(0,5),
           child = "mean") %>%
  dag_node("Sepal / Petal Slope","coef",
           rhs = normal(0,3),
           child = "mean") %>%
  dag_node("Std Dev of Obs. Sepal Length","sd",
           rhs = lognormal(0,3),
           child = "y") %>%
  dag_node("Petal Length","x",
           child = "mean",
           data = iris$Petal.Length) %>%
  dag_plate("Observation","i",
           nodeLabels = c("y","mean","x"))
graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_greta()
graph %>% dag_greta(mcmc = TRUE)
drawsDF %>% dagp_plot()

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

### greta example #3
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

### greta example #4
df = as.data.frame(model.matrix(breaks ~ wool + tension, warpbreaks))
graph = dag_create() %>%
  dag_node("# of Breaks Per Loom","y",
           rhs = poisson(exp(eta)),
           data = warpbreaks$breaks) %>%
  dag_node("Linear Predictor","eta",
           rhs = X %*% beta,
           child = "y") %>%
  dag_node(descr = "Wool + Tension Data", label = "X",
           data = df,
           keepAsDF = TRUE,
           child = "eta") %>%
  dag_node("Model Coef. Vector","beta",
           rhs = c(int, coefs),
           child = "eta") %>%
  dag_node("Intercept","int",
           rhs = variable,
           child = "beta") %>%
  dag_node("Slope Coefficients","coefs",
           rhs = normal(0,5),
           child = "beta") %>%
  dag_plate("Predictors","j",
            data = names(df[ ,-1]),
            nodeLabels = "coefs",
            addDataNode = FALSE) %>%
  dag_plate("Observations","i",
            nodeLabels = c("y","eta"))


graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_greta()
graph %>% dag_greta(mcmc = TRUE)
drawsDF %>% dagp_plot()
