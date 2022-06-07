test_that("edge is created", {
  listObj1 = dag_create() %>%
    dag_node("Child Node","y") %>%
    dag_node("Parent Node","x", child = "y")
  listObj2 = dag_create() %>%
    dag_node("Child Node","y") %>%
    dag_node("Parent Node","x") %>%
    dag_edge("x","y")
  expect_equal(listObj1$edges_df$from, 2)
  expect_equal(listObj2$edges_df$from, 2)
  expect_equal(listObj1$edges_df$to, 1)
  expect_equal(listObj2$edges_df$to, 1)
})
