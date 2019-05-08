


library("rstanarm")
library("bayesplot")
library("loo")

data(roaches)
str(roaches)

roaches$roach1 <- roaches$roach1 / 100

fit1 <-
  stan_glm(
    formula = y ~ roach1 + treatment + senior,
    offset = log(exposure2),
    data = roaches,
    family = poisson(link = "log"),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_intercept = normal(0, 5, autoscale = TRUE),
    seed = 12345
  )

loo1 <- loo(fit1, save_psis = TRUE)
print(loo1)

library(greta)
library(tidyverse)

X <- roaches %>%
  select(roach1,treatment,senior)
x_greta <- as_data(X)
y_greta <- as_data(roaches$y)

a <- normal(0, 5)
b1 <- normal(0, 2.5)
b2 <- normal(0, 2.5)
b3 <- normal(0, 2.5)
y <- a+ b1*X$roach1 + b2*X$treatment + b3*X$senior
lambda <- exp(y)
distribution(y_greta) <- poisson(lambda)

m_greta <- model(a,b1,b2,b3)
plot(m_greta)

draw_m_greta <- mcmc(m_greta, warmup = 3000, n_samples = 1000)
draw_trace <- mcmc_trace(draw_m_greta)
draw_pairs <- mcmc_pairs(draw_m_greta)
draw_trace
draw_pairs

summary(draw_m_greta)
