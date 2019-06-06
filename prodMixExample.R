library(causact)
library(greta)

## proudct mix problem
dag_create() %>%
  dag_node("Demand for A","dA",
           rhs = normal(15,4,truncation = c(0,30))) %>%
  dag_node("Demand for B","dB",
           rhs = normal(20,8,truncation = c(0,30))) %>%
  dag_node("Supply for A","sA",
           rhs = uniform(0,100)) %>%
  dag_node("Supply for B","sB",
           rhs = uniform(0,100)) %>%
  dag_node("Profit for A","pA",
           rhs = min(sA,dA)) %>%
  dag_node("Profit for B","pB",
           rhs = min(sA,dA))  %>%
  dag_edge(from = c("dA","sA"),to = c("pA")) %>%
  dag_edge(from = c("dB","sB"),to = c("pB")) %>%
  dag_node("Total Profit", "total",
           rhs = pA + pB) %>%
  dag_edge(from = c("pA","pB"), to = "total") %>%
  dag_node("Total Profit Dist", "totDist",
           rhs = beta(3,1)) %>%
  dag_node("Total Profit Guess","totGuess",
           rhs = 60 * totDist) %>%
  dag_edge("totDist","totGuess") %>%
  dag_render()


graph %>% dag_greta(mcmc=TRUE)
tidyDrawsDF %>% dagp_plot()



#newsvendor
## proudct mix problem
dag_create() %>%
  dag_node("Demand for A","dA",
           rhs = normal(mu,sigma),
           data = runif(20,10,20)) %>%
  dag_node("Exp Demand for A", "mu",
           rhs = normal(15,4),
           child = "dA") %>%
  dag_node("Std Dev of Demand", "sigma",
           rhs = uniform(0,8),
           child = "dA") %>%
  dag_node("Supply for A","supply",
           rhs = mu + 1.5 * sigma) %>%
  dag_edge(c("mu","sigma"),"supply") %>%
  #dag_render()
  dag_greta(mcmc=TRUE)
