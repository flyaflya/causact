library(causact)

dag_create() %>%
  dag_node("Demand") %>%
  dag_node("Inventory",
           dec = TRUE) %>%
  dag_node("Revenue") %>%
  dag_edge("Demand","Revenue") %>%
  dag_edge("Inventory","Revenue") %>%
  dag_render(shortLabel = TRUE)

