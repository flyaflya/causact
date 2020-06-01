library(causact)
library(greta)

revFunction = function(custLoc,bathLoc) {
  rev = abs(custLoc - bathLoc)
}


graph = dag_create() %>%
  dag_node("Revenue","x",
           rhs = revFunction(y,z)) %>%
  dag_node("Beachgoer Location","y",
           child = "x",
           rhs = normal(70,15)) %>%
  dag_node("Bathhouse Location","z",
           child = "x",
           data = 70,
           dec = TRUE)

graph %>% dag_render(shortLabel = TRUE,
                     wrapWidth = 15)

graph %>% dag_greta(mcmc=TRUE)


graph = dag_create() %>%
  dag_node("Revenue") %>%
  dag_node("Beachgoer Location",
           child = "Revenue") %>%
  dag_node("Bathhouse Location",
           child = "Revenue",
           dec = TRUE)

graph %>% dag_render(shortLabel = TRUE,
                     wrapWidth = 15)

posterior_sample = function(n) {
  dist_i = sample(4, n, replace = TRUE, c(35, 15, 10, 60))
  mu = c(0.2, 0.9, 1.1, 2.5)
  sigma = c(0.25, 0.6, 0.15, 0.5)
  post = rnorm(n, mu[dist_i], sigma[dist_i])
  post = post[post >= 0 & post <= 2.6]
  post = floor(post*10)/10
  return(post)
}
# The posterior sample
s <- posterior_sample(99999)

totalBeachgoersRepSample = as.integer(round(rgamma(n = 4000,shape = 400, scale = 20) + rexp(1000,1/1000),0))

usethis::use_data(totalBeachgoersRepSample)
