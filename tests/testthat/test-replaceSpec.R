test_that("handle Latex in graphviz", {
  testthat::expect_no_error(
    dag_create() %>%
      dag_node("X\u2080") %>%
      dag_render())
})

