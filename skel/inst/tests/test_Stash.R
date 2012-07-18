context("Stash")

test_that("Stash", {
  reg = makeTestRegistry()
  putInStash(reg, "foo", 1:2)
  putInStash(reg, "bar", 99)
  expect_equal(getFromStash(reg, "foo")[[1L]], 1:2)
  expect_error(putInStash(reg, "foo", 1:2))

  addProblem(reg, "p1", static = 1, dynamic = function(stash) stash$get("foo"))
  addAlgorithm(reg, "a1", fun = function(dynamic) dynamic)
  id = addExperiments(reg)
  submitJobs(reg)
  expect_equal(loadResult(reg, id), 1:2)

  addProblem(reg, "p2", 1)
  addAlgorithm(reg, "a2", fun = function(static, stash) stash$get("foo"))
  id = addExperiments(reg, "p2", "a2")
  submitJobs(reg)
  expect_equal(loadResult(reg, id), 1:2)

  addAlgorithm(reg, "a3", fun = function(static, stash) stash$list())
  id = addExperiments(reg, "p2", "a3")
  submitJobs(reg)
  res = loadResult(reg, id)
  expect_equal(sort(res), c("bar", "foo"))
})
