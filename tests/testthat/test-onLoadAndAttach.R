testthat::test_that(".onLoad() is called without errors", {
  # Load your package
  library(causact)

  # Test if .onLoad() runs without errors
  testthat::expect_no_error(.onLoad(ns, exports))
})

testthat::test_that(".onAttach() is called without errors", {
  # Load your package
  library(causact)

  # Test if .onAttach() runs without errors
  testthat::expect_no_error(.onAttach(NULL))
})
