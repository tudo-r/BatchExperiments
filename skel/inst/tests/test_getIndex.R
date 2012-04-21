context("getIndex")

test_that("getIndex", {
  r = makeTestRegistry()
  p1 = addProblem(r, "one", 1)
  p2 = addProblem(r, "two", 2)
  a1 = addAlgorithm(r, "A", fun=function(static, dynamic) 1)
  a2 = addAlgorithm(r, "B", fun=function(static, dynamic) 1)
  addExperiments(r, list(makeDesign(p1), makeDesign(p2)), list(makeDesign(a1), makeDesign(a2)), repls=2)

  expect_equal(getIndex(r, by.prob=TRUE),
               list(prob = factor(rep(c("one", "two"), each = 4))))
  expect_equal(getIndex(r, by.algo=TRUE),
               list(algo = factor(rep(rep(c("A", "B"), each = 2), 2))))
  expect_true(length(getIndex(r, by.algo=TRUE, by.prob=TRUE)) == 2L)
  expect_true(length(getIndex(r, ids=integer(0))) == 0)
  expect_true(length(getIndex(r, ids=integer(0), by.prob=TRUE)) == 1)
  #FIXME more tests
})
