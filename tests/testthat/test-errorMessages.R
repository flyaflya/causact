testthat::test_that("graph can be rendered", {
  renderGraph = dag_create() %>%
    dag_node("x") %>% dag_node("y") %>% dag_render()

  ## send rendered graph to graph construction functions
  f1 <- function() {renderGraph %>% dag_node("z")}
  testthat::expect_error(f1())
  f1 <- function() {renderGraph %>% dag_edge("x","y")}
  testthat::expect_error(f1())
  f1 <- function() {renderGraph %>%
      dag_plate(label = "i",descr = "Obs",
                nodeLabels = "x")}
  testthat::expect_error(f1())
  f1 <- function() {renderGraph %>%
      dag_render()}
  testthat::expect_error(f1())
  f1 <- function() {renderGraph %>%
      dag_numpyro()}
  testthat::expect_error(f1())

  ## send other object to graph construction functions
  renderGraph = "x"
  f1 <- function() {renderGraph %>% dag_node("z")}
  testthat::expect_error(f1())
  f1 <- function() {renderGraph %>% dag_edge("x","y")}
  testthat::expect_error(f1())
  f1 <- function() {renderGraph %>%
      dag_plate(label = "i",descr = "Obs",
                nodeLabels = "x")}
  testthat::expect_error(f1())
  f1 <- function() {renderGraph %>%
      dag_render()}
  testthat::expect_error(f1())
  f1 <- function() {renderGraph %>%
      dag_numpyro()}
  testthat::expect_error(f1())
})


