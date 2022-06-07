test_that("data frame is created", {
  listObj = dag_create()
  expect_type(listObj, "list")
})

