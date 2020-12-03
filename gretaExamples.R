### Script contains five examples from
### https://greta-stats.org/articles/example_models.html

library(greta)
library(tidyverse)
library(causact)

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
graph %>% dag_greta(mcmc = FALSE)
drawsDF = graph %>% dag_greta()
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
drawsDF = graph %>% dag_greta()
drawsDF %>% dagp_plot()


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

graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
drawsDF = graph %>% dag_greta()
drawsDF %>% dagp_plot()

### greta example #3
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
           rhs = normal(0,10),
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

graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
drawsDF = graph %>% dag_greta()
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
           rhs = variable(-Inf,Inf),
           child = "beta") %>%
  dag_node("Slope Coefficients","coefs",
           rhs = normal(0,5),
           child = "beta",
           extract = FALSE) %>%
  dag_plate("Predictors","j",
            data = names(df[ ,-1]),
            nodeLabels = "coefs") %>%
  dag_plate("Observations","i",
            nodeLabels = c("y","eta"))


graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
drawsDF = graph %>% dag_greta()
drawsDF %>% dagp_plot()

### greta example #5: Multiple Categorical Regression
designDF = as.data.frame(model.matrix(~ Species -1, iris))
speciesCoding = colnames(designDF) %>% head(-1)
graph = dag_create() %>%
  dag_node("Species","y",
           data = designDF,
           keepAsDF = TRUE,
           rhs = categorical(prob)) %>%
  dag_node("Category Probabilities","prob",
           child = "y",
           rhs = imultilogit(eta)) %>%
  dag_node("Linear Predictor","eta",
           rhs = X %*% beta,
           child = "prob") %>%
  dag_node("Predictors","X",
           data = cbind(1,iris[, 1:4]),
           keepAsDF = TRUE,
           child = "eta") %>%
  dag_node("Predictor Coefficients","beta",
           rhs = normal(0,5),
           child = "eta",
           extract = FALSE) %>%
  dag_plate(descr = "Predictors",label = "J",
            nodeLabels = "beta",
            data = c("Intercept",colnames(iris[,1:4]))) %>%
  dag_plate("Category Codes","K",
            nodeLabels = "beta",
            data = speciesCoding)

graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
drawsDF = graph %>% dag_greta()
drawsDF %>% dagp_plot()

