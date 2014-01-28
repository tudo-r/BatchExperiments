context("testJob")
test_that("testJob", {
  reg = makeTestRegistry()
  p1 = addProblem(reg, "p1", 1)
  a1 = addAlgorithm(reg, id="a1", fun=function(static, dynamic) static)
  addExperiments(reg, p1, a1)
  expect_equal(testJob(reg, 1L, external=FALSE), 1)
})
