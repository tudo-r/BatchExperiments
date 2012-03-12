context("summarizeExperiments")

test_that("summarizeExperiments", {
  r = makeTestRegistry()
  p1 = addProblem(r, "one", 1)
  p2 = addProblem(r, "two", 2)
  a1 = addAlgorithm(r, "A", fun=function(static, dynamic) 1)
  a2 = addAlgorithm(r, "B", fun=function(static, dynamic) 1)
  addExperiments(r, c("one", "two"), c("A", "B"))
  expect_equal(summarizeExperiments(r), NULL)
})
