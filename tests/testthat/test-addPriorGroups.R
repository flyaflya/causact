test_that("addPriorGroups", {
  priorGroupDF = data.frame(label = c("x","y"),
                            priorGroup=1)
  assign("priorGroupDF",priorGroupDF, cacheEnv)
  assign("meaningfulLabels",data.frame(rootLabel = c("x","y"), newNames = c("x","y")), cacheEnv)
  drawsDF = data.frame(x=c(4,1),y=c(10,2))
  testthat::expect_no_error(causact:::addPriorGroups(drawsDF))

})

