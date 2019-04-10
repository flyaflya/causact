library(greta)
coinflip = rbinom(n = 1000,size = 1,prob = 0.6)
y <- as_data(coinflip)   #DATA
theta  <- beta(shape1 = 2, shape2 = 2)   #PRIOR
distribution(y) <- bernoulli(prob = theta)   #LIKELIHOOD
gretaModel <- model(theta)   #MODEL
system.time(draws <- mcmc(gretaModel))   #POSTERIOR
