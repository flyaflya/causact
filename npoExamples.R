### Script contains five examples from
### https://greta-stats.org/articles/example_models.html

library(tidyverse)
library(causact)

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

graph %>% dag_render()
drawsDF = graph %>% dag_numpyro()
drawsDF %>% dagp_plot()
drawsDF %>% dagp_plot(abbrevLabels = TRUE)
drawsDF %>% dagp_plot(densityPlot = TRUE)
drawsDF %>% dagp_plot(densityPlot = TRUE, abbrevLabels = TRUE)

drawsDF %>%
  mutate(indicatorFunction =
           theta_Kia.Forte > theta_Toyota.Corolla) %>%
  summarize(pctOfDraws = mean(indicatorFunction))

### greta example 0
graph = dag_create() %>%
  dag_node("Sepal Length","y",
           data = iris$Sepal.Length,
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
library(tidyverse)
graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_numpyro(mcmc = FALSE)
drawsDF = graph %>% dag_numpyro()
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
drawsDF = graph %>% dag_numpyro()
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
  dag_node("Intercept","int",
           rhs = normal(0,10),
           child = "mu") %>%
  dag_plate(descr = "Predictors",label = "j",
            nodeLabels = c("coefs"),
            data = names(attitude[, 2:7])) %>%
  dag_plate(descr = "Observation", label = "i",
            nodeLabels = c("mu","y"))

graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
drawsDF = graph %>% dag_numpyro()
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
drawsDF = graph %>% dag_numpyro()
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
           rhs = normal(0,10000),
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
drawsDF = graph %>% dag_numpyro()
drawsDF %>% dagp_plot()

### greta example #5: Multiple Categorical Regression
### NOT SUPPORTED YET
designDF = as.data.frame(model.matrix(~ Species -1, iris))
speciesCoding = colnames(designDF) %>% head(-1)
graph = dag_create() %>%
  dag_node("Species","y",
           data = designDF,
           keepAsDF = TRUE,
           rhs = categorical(prob)) %>%
  dag_node("Category Probabilities","prob",
           child = "y",
           #rhs = imultilogit(eta)
           ) %>%
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
            nodeLabels = c("beta"),
            data = speciesCoding)

graph %>% dag_render()
graph %>% dag_render(shortLabel = TRUE)
drawsDF = graph %>% dag_numpyro(mcmc = FALSE)
drawsDF %>% dagp_plot()

### using mixture example
# simulate a mixture of poisson random variables and try to recover the
# parameters with a Bayesian model
x <- c(rpois(800, 3),
       rpois(200, 10))

# weights <- uniform(0, 1, dim = 2)
# rates <- normal(0, 10, truncation = c(0, Inf), dim = 2)
# distribution(x) <- mixture(poisson(rates[1]),
#                            poisson(rates[2]),
#                            weights = weights)

graph2 = dag_create() %>%
  dag_node("Mixed Var","x",
           rhs = mixture(alpha, beta, weights = weights),
           data = x) %>%
  dag_node("Alpha Var","alpha",
           rhs = poisson(lambda_a),
           child = "x") %>%
  dag_node("Beta Var","beta",
           rhs = poisson(lambda_b),
           child = "x") %>%
  dag_node("Weight Vars","weights",
           rhs = uniform(0,1, dim =2),
           child = "x") %>%
  dag_node("Lambda A Rate","lambda_a",
           rhs = uniform(1,5),
           child = "alpha") %>%
  dag_node("Lambda B Rate","lambda_b",
           rhs = uniform(6,20),
           child = "beta")
graph2 %>% dag_render()
graph2 %>% dag_numpyro(mcmc=FALSE)
drawsDF = graph2 %>% dag_numpyro()
drawsDF %>% dagp_plot()

#### use dirichlet instead
library(greta)
library(tidyverse)
library(causact)

## sample data - try to recover params
x <- c(rpois(800, 3),rpois(200, 10))

graph2 = dag_create() %>%  ## create generative DAG
  dag_node("Mixed Var","x",
           rhs = mixture(alpha,beta,
                         weights = t(weights)),
           data = x) %>%
  dag_node("Count Var 1","alpha",
           rhs = poisson(lambda1),
           child = "x") %>%
  dag_node("Count Var 2","beta",
           rhs = poisson(lambda2),
           child = "x") %>%
  dag_node("Weight Vars","weights",
           rhs = dirichlet(t(c(1,1))),
           child = "x") %>%
  dag_node("Exp Rate 1","lambda1",
           rhs = uniform(1,5),
           child = "alpha") %>%
  dag_node("Exp Rate 2","lambda2",
           rhs = uniform(6,20),
           child = "beta")
graph2 %>% dag_render()  ## visualize DAG
drawsDF = graph2 %>% dag_numpyro() ## get posterior
drawsDF %>% dagp_plot() ## visualize posterior
