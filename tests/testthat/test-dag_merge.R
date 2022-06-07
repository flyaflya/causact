test_that("dag merge can combine graphs", {
  g1 = dag_create() %>%
    dag_node("Demand for A","dA",
             rhs = normal(15,4)) %>%
    dag_node("Supply for A","sA",
             rhs = uniform(0,100)) %>%
    dag_node("Profit for A","pA",
             rhs = min(sA,dA)) %>%
    dag_edge(from = c("dA","sA"),to = c("pA"))
  g2 <- dag_create() %>%
    dag_node("Demand for B","dB",
             rhs = normal(20,8)) %>%
    dag_node("Supply for B","sB",
             rhs = uniform(0,100)) %>%
    dag_node("Profit for B","pB",
             rhs = min(sB,dB)) %>%
    dag_edge(from = c("dB","sB"),to = c("pB"))
  newGraph = g1 %>% dag_merge(g2)
  expect_equal(NROW(newGraph), 6)
})
