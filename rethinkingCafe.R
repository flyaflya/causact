library(tidyverse)
library(greta)
# Simulated cafe data
a <- 3.5 # average morning wait time 14.1
b <- (-1) # average difference afternoon wait time
sigma_a <- 1 # std dev in intercepts
sigma_b <- 0.5 # std dev in slopes
rho <- (-0.7) # correlation between intercepts and slopes

# Vector of means
Mu <- c( a , b )

# Covaraince matrix
sigmas <- c(sigma_a,sigma_b) # standard deviations
Rho <- matrix( c(1,rho,rho,1) , nrow=2 ) # correlation matrix

# now matrix multiply to get covariance matrix
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)

# Simulate cafes
N_cafes <- 20
library(MASS)
set.seed(5) # used to replicate example
vary_effects <- mvrnorm( N_cafes , Mu , Sigma )

a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]

library(tidyverse)
ggplot(data.frame(a_cafe = a_cafe, b_cafe = b_cafe), aes(x=a_cafe,y=b_cafe)) + geom_point()

# Simulate observations
N_visits <- 10
afternoon <- rep(0:1,N_visits*N_cafes/2)
cafe_id <- rep( 1:N_cafes , each=N_visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- 0.5 # std dev within cafes
wait <- rnorm( N_visits*N_cafes , mu , sigma )
waitDF <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )


# create DAG using causact package
graph = dag_create() %>%
  dag_node("Obs Wait Time","x",
           data = waitDF$wait,
           rhs = normal(mu,sig)) %>%
  dag_node("Exp Obs Wait Time","mu",
           rhs = alpha_cafe + beta_cafe * afternoon,
           child = "x") %>%
  dag_node("Exp Morning Wait Time","alpha_cafe",
           child = "mu",
           rhs = Y[,1],
           extract = TRUE) %>%
  dag_node("Exp Wait Time Diff.","beta_cafe",
           child = "mu",
           rhs = Y[,2],
           extract = TRUE) %>%
  dag_node("Cafe","cafe",
           child = "mu",
           data = waitDF$cafe) %>%
  dag_node("Afternoon","afternoon",
           child = "mu",
           data = waitDF$afternoon) %>%
  dag_node("Exp Wait and Wait Diff","Y",
           rhs = multivariate_normal(Mean,Sigma),
           child = c("alpha_cafe", "beta_cafe")) %>%
  dag_node("Uncorr. Exp. Effects","Mean",
           rhs = cbind(alpha,beta),
           child = "Y") %>%
  dag_node("Covar Matrix","Sigma",
           rhs = Sigmas %*% Rho %*% Sigmas,
           child = "Y") %>%
  dag_node("Uncorr. Morning Wait","alpha",
           rhs = normal(0,10),
           child = "Mean",
           extract = FALSE) %>%
  dag_node("Uncorr. Wait Diff","beta",
           rhs = normal(0,10),
           child = "Mean",
           extract = FALSE) %>%
  dag_node("Uncorr Std Devs","Sigmas",
           child = "Sigma",
           rhs = makeDiagMatrix(c(sig_a,sig_b))) %>%
  dag_node("Obs Wait Std Dev","sig",
           child = "x",
           rhs = cauchy(0,1,truncation = c(0,Inf))) %>%
  dag_node("Morning Wait Std Dev","sig_a",
           child = "Sigmas",
           rhs = cauchy(0,1,truncation = c(0,Inf))) %>%
  dag_node("Wait Diff Std Dev","sig_b",
           child = "Sigmas",
           rhs = cauchy(0,1,truncation = c(0,Inf))) %>%
  dag_node("Wait Corr. Matrix","Rho",
           child = "Sigma",
           rhs = lkj_correlation(2)) %>%
  dag_plate("Observation","j",
            nodeLabels = c("x","mu","cafe","afternoon")) %>%
  dag_plate("Cafes","cafe",
            nodeLabels = c("alpha","beta","alpha_cafe","beta_cafe"),
            data = waitDF$cafe) %T>%
  dag_render()

graph %>% dag_render(width = 2400, height = 600)
graph %>% dag_render(shortLabel = TRUE)
graph %>% dag_greta()
### alter greta code to run:
# Sigmas <- zeros(2,2)
# diag(Sigmas) <- c(sig_a,sig_b)
# Sigma <- Sigmas %*% Rho %*% Sigmas
### and flip flop order of some statements
graph %>% dag_greta(mcmc=TRUE, one_by_one = TRUE)
tidyDrawsDF %>% dagp_plot()

graph = dag_create() %>%
  dag_node("Obs Wait Time","x") %>%
  dag_render()
