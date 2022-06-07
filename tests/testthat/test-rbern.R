test_that("bernoulli rvs can be generated", {
  expect_equal(sum(rbern(10,1)), 10)
})
