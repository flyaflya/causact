library(testthat)
library(causact)
library(greta)



RunningTest <- function() {


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
tidyDrawsDF %>% dagp_plot()
DF0 = tidyDrawsDF

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
tidyDrawsDF %>% dagp_plot()
DF1 = tidyDrawsDF

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
graph %>% dag_greta()
graph %>% dag_greta(mcmc = TRUE)
tidyDrawsDF %>% dagp_plot()
DF2 = tidyDrawsDF

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
graph %>% dag_greta()
graph %>% dag_greta(mcmc = TRUE)
tidyDrawsDF %>% dagp_plot()
DF3 = tidyDrawsDF

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
graph %>% dag_greta()
graph %>% dag_greta(mcmc = TRUE)
tidyDrawsDF %>% dagp_plot()
DF4 = tidyDrawsDF


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
graph %>% dag_greta()
graph %>% dag_greta(mcmc = TRUE)
tidyDrawsDF %>% dagp_plot()
DF5 = tidyDrawsDF


### greta example #6: Multiple Possion Regression
data("warpbreaks")
x <- as_data(model.matrix(breaks ~ wool + tension, warpbreaks))
y <- as_data(warpbreaks$breaks)

graph = dag_create() %>%
  dag_node("final",label="poisson",rhs=poisson(exp(eta)), data=y) %>%
  dag_node("eta",label="eta", rhs=x%*%beta, child="final") %>%
  dag_node("x", label="x", data=x, child="eta") %>%
  dag_node("beta",label="beta", rhs=c(int,coefs), child="eta") %>%
  dag_node("intercept",label="int", rhs=variable(-Inf,Inf), child="beta") %>%
  dag_node("coefficient",label="coefs", rhs=normal(0,5,dim=(ncol(x)-1)), child="beta")

graph %>% dag_render()
graph %>% dag_greta(mcmc=TRUE)
DF6 = tidyDrawsDF



### greta example #7: Hierarchical Linear Regression in General Conditional Formulation
n_species  <- length(unique(iris$Species))
species_id <- as.numeric(iris$Species)
Z <- model.matrix(~ Species + Sepal.Length * Species - 1, data = iris)
gamma_matrix <- multivariate_normal(matrix(0, 1, 2), diag(2), n_realisations = 3)
gamma <- c(gamma_matrix)
wi <- as_data(iris$Sepal.Width)
Z  <- as_data(Z)


graph = dag_create() %>%
  dag_node("dist", label="dist", rhs=normal(mu,sd), data=iris$Sepal.Length) %>%
  dag_node("mu", label="mu", rhs=int+coef*wi+Z%*%gamma, child="dist") %>%
  dag_node("int", label="int", rhs=normal(0,10), child="mu") %>%
  dag_node("coef", label="coef", rhs=normal(0,10), child="mu") %>%
  dag_node("sd", label="sd", rhs=cauchy(0,3,truncation=c(0,Inf)), child="dist")
graph %>% dag_render()
graph %>% dag_greta(mcmc=TRUE)
DF7 = tidyDrawsDF


### greta example #8: Beyasian Neural Network
N <- 100
p <- 10
y <- matrix(rnorm(N * p), N)%*%rnorm(10) +rnorm(N, sd=0.1)
set.seed(23)
graph = dag_create() %>%
  dag_node("output", label="output", rhs=normal(x%*%weights, sd), data=y) %>%
  dag_node("weights", label="weights", rhs=normal(0,1,dim=c(p,1)), child="output") %>%
  dag_node("x", label="x", rhs=matrix(rnorm(N*p),N), child="output") %>%
  dag_node("sd", label="sd", rhs=inverse_gamma(1,1), child="output") #%>%
#dag_node("y", label="y", rhs=matrix(rnorm(N * p), N)%*%rnorm(10) +rnorm(N, sd=0.1), child="output")
graph %>% dag_render()
graph %>% dag_greta(mcmc=TRUE)
DF8 = tidyDrawsDF

return (c(DF0, DF1, DF2, DF3, DF4, DF5, DF6, DF7, DF8))
}

#Run the test function to get the results from the examples
data = RunningTest()

# Prepare the benchmark data for comparison, this step does not run for testing procedure
#saveRDS(data, "benchmark.rds")

#Every time run the testing code, it will read the saved benchmark for further comparison
benchmark = readRDS("benchmark.rds")

#This is the comparison step
expect_equal(data,benchmark)

