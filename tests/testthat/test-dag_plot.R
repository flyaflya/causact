test_that("dag plot creates graph", {
  df = data.frame(x = c(1:5),y = c(6:10))
  plotGr = df %>% dagp_plot()
  expect_type(plotGr, "list")
})

