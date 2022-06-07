test_that("nodes are created", {
  listObj = dag_create() %>%
    dag_node("Child Node","y") %>%
    dag_node("Parent Node","x")
  expect_type(listObj, "list")
  expect_equal(listObj$nodes_df$descr[1], "Child Node")
  expect_equal(listObj$nodes_df$descr[2], "Parent Node")
})
