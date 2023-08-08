test_that("distributions have good output", {
  normString = causact::normal(0,10)
  expect_equal(normString, "dist.Normal(0,10)")
  expString = causact::exponential(7)
  expect_equal(expString, "dist.Exponential(7)")
  uniformString = causact::uniform(0,8)
  expect_equal(uniformString, "dist.Uniform(0,8)")
  studentString = causact::student(df=3,mu=8,sigma=7)
  expect_equal(studentString, "dist.StudentT(3,8,7)")
})

