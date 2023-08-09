test_that("dag plot creates graph", {
  df = data.frame(x = c(1:5),y = c(6:10))
  plotGr = df %>% dagp_plot()
  expect_type(plotGr, "list")
  testthat::expect_no_error(
    df %>% dagp_plot(densityPlot = TRUE)
  )
})

