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
  dag_node("Revenue",
           rhs = x+ 4) %>%
  dag_node("Beachgoer Location",
           child = "Revenue") %>%
  dag_node("Bathhouse Location",
           child = "Revenue",
           dec = TRUE)

graph %>% dag_render(shortLabel = TRUE,
                     wrapWidth = 15)
