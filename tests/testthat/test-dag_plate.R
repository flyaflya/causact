test_that("plate is created", {
  graph = dag_create() %>%
    dag_node("Child Node","y") %>%
    dag_node("Parent Node","x") %>%
    dag_edge("x","y") %>%
    dag_plate("Observation","i",
              nodeLabels = "y")
  expect_type(graph, "list")
  expect_equal(graph$nodes_df$descr[1], "Child Node")
  expect_equal(graph$nodes_df$descr[2], "Parent Node")
  expect_equal(graph$edges_df$from, 2)
  expect_equal(graph$edges_df$from, 2)
  expect_equal(graph$edges_df$to, 1)
  expect_equal(graph$edges_df$to, 1)
  expect_equal(graph$plate_index_df$indexDescription, "Observation")
  expect_equal(graph$plate_node_df$nodeID, 1)
})
