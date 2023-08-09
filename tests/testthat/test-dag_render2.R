test_that("double plate graph can be rendered", {
  renderGraph = dag_create() %>%
    dag_node("# of Hot Day Beachgoers") %>%
    dag_node(c("Rent by Location",
               "Beachgoer Location Probability"),
             obs = TRUE) %>%
    dag_node(c("Revenue","Expenses","Profit"),
             det = TRUE) %>%
    dag_node("Bathhouse Location",
             dec = TRUE) %>%
    dag_edge(from = c("# of Hot Day Beachgoers",
                      "Beachgoer Location Probability",
                      "Bathhouse Location"),
             to = "Revenue") %>%
    dag_edge(from = c("Bathhouse Location",
                      "Rent by Location"),
             to = "Expenses") %>%
    dag_edge(from = c("Revenue","Expenses"),
             to = "Profit") %>%
    dag_plate("plate1","plate1",nodeLabels = "Revn") %>%
    dag_plate("plate2","plate2",nodeLabels = "Expn") %>%
    dag_render()
  expect_type(renderGraph, "list")
})
