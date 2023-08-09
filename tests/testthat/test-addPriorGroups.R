test_that("addPriorGroups", {

  drawsDF = data.frame(x=4,y=10)
  out = addPriorGroups(drawsDF)
  expect_type(out, "list")
})

