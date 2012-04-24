context("summarizeExperiments")

test_that("summarizeExperiments", {
  r = makeTestRegistry()
  p1 = addProblem(r, "one", 1)
  p2 = addProblem(r, "two", 2)
  a1 = addAlgorithm(r, "A", fun=function(static, dynamic) 1)
  a2 = addAlgorithm(r, "B", fun=function(static, dynamic) 1)
  addExperiments(r, c("one", "two"), c("A", "B"))
  expect_equal(dim(summarizeExperiments(r)), c(2, 2))
  expect_equal(dim(summarizeExperiments(r, ids=integer(0L))), c(0, 0))
})
