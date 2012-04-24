context("addExperiments")

test_that("addExperiments", {
  # 1 prob 1 algo
  reg = makeTestRegistry()
  p1 = addProblem(reg, "p1", 1)
  pd1 = makeDesign(p1)
  a1 = addAlgorithm(reg, id="a1", fun=function(static, dynamic) static)
  expect_equal(getProblemIds(reg), "p1")
  expect_equal(getAlgorithmIds(reg), "a1")
  ad1 = makeDesign(a1)
  addExperiments(reg, prob.designs=pd1, algo.designs=ad1)
  submitJobs(reg)
  id = getJobIds(reg)[1]
  res = loadResult(reg, id)
  expect_equal(res, 1)
  removeExperiments(reg, force=TRUE)
  expect_equal(findExperiments(reg), id)
  expect_equal(findMissingResults(reg), integer(0))
  loadResult(reg, id)
  expect_output({
    d = showStatus(reg)
  }, "Status for jobs: 1")
  removeExperiments(reg, id, force=TRUE)
  expect_equal(findExperiments(reg), integer(0))
  expect_equal(findMissingResults(reg), integer(0))
  expect_error(loadResult(reg, id), "Ids not present")
  expect_output({
    d = showStatus(reg)
  }, "")
  expect_equal(d$n, 0)

  # 2 prob 1 algo
  reg = makeTestRegistry()
  p1 = addProblem(reg, "p1", 1)
  p2 = addProblem(reg, "p2", 2)
  a1 = addAlgorithm(reg, id="a1", fun=function(static, dynamic) static)
  expect_equal(getProblemIds(reg), c("p1", "p2"))
  expect_equal(getAlgorithmIds(reg), "a1")
  pd2 = makeDesign(p2)
  addExperiments(reg, list(pd1, pd2), ad1)
  submitJobs(reg)
  res = sapply(getJobIds(reg), loadResult, reg=reg)
  expect_equal(res, 1:2, check.attributes=FALSE)

  # algo with pars
  reg = makeTestRegistry()
  p1 = addProblem(reg, "p1", 1)
  a2 = addAlgorithm(reg, id="a2", fun=function(static, dynamic, a) static+a)
  ad2 = makeDesign(a2, exhaustive=list(a=1:2))
  addExperiments(reg, pd1, ad2)
  submitJobs(reg)
  res = sapply(getJobIds(reg), loadResult, reg=reg)
  expect_equal(res, (1:2)+1,, check.attributes=FALSE)

  # prob with pars
  reg = makeTestRegistry()
  p3 = addProblem(reg, "p3", static=5, dynamic=function(static, a) static+a)
  pd3 = makeDesign(p3, exhaustive=list(a=1:2))
  a3 = addAlgorithm(reg, id="a3", fun=function(static, dynamic) static+dynamic)
  ad3 = makeDesign(a3)
  addExperiments(reg, pd3, ad3)
  submitJobs(reg)
  res = sapply(getJobIds(reg), loadResult, reg=reg)
  expect_equal(res, c(2*5+1, 2*5+2), check.attributes=FALSE)

  submitLoadAndCheck = function(reg, m) {
    submitJobs(reg)
    res = sapply(getJobIds(reg), loadResult, reg=reg)
    res = unique(res)
    expect_true(length(res) == m && is.numeric(res) && res >= 0 && res <= 1)
  }

  #prob seed. 1 stochastic prob without seed, 1 algo, 2 reps = 2 different probs
  reg = makeTestRegistry()
  p4a = addProblem(reg, "p4a", static=5, dynamic=function(static) runif(1))
  a4 = addAlgorithm(reg, id="a4", fun=function(static, dynamic) dynamic)
  addExperiments(reg, makeDesign(p4a), makeDesign(a4), 2)
  submitLoadAndCheck(reg, 2)

  #prob seed. 1 stochastic prob with seed, 1 algo, 2 reps = 2 different probs
  reg = makeTestRegistry()
  p4b = addProblem(reg, "p4a", static=5, dynamic=function(static) runif(1), seed=1)
  a4 = addAlgorithm(reg, id="a4", fun=function(static, dynamic) dynamic)
  addExperiments(reg, makeDesign(p4b), makeDesign(a4), 2)
  submitLoadAndCheck(reg, 2)

  #prob seed. 1 stochastic prob without seed, 2 algos, 2 reps = 4 different probs
  reg = makeTestRegistry()
  p4a = addProblem(reg, "p4a", static=5, dynamic=function(static) runif(1))
  a4 = addAlgorithm(reg, id="a4", fun=function(static, dynamic) dynamic)
  a5 = addAlgorithm(reg, id="a5", fun=function(static, dynamic) dynamic)
  addExperiments(reg, makeDesign(p4a), list(makeDesign(a4), makeDesign(a5)), 2)
  submitLoadAndCheck(reg, 4)

  #prob seed. 1 stochastic prob with seed, 2 algos, 2 reps = 2 different probs
  reg = makeTestRegistry()
  p4b = addProblem(reg, "p4a", static=5, dynamic=function(static) runif(1), seed=1)
  a4 = addAlgorithm(reg, id="a4", fun=function(static, dynamic) dynamic)
  a5 = addAlgorithm(reg, id="a5", fun=function(static, dynamic) dynamic)
  addExperiments(reg, makeDesign(p4b), list(makeDesign(a4), makeDesign(a5)), 2)
  submitLoadAndCheck(reg, 2)

  # check that same exps cannot be added again
  reg = makeTestRegistry()
  p1 = addProblem(reg, "p1", 1)
  pd1 = makeDesign(p1)
  a1 = addAlgorithm(reg, id="a1", fun=function(static, dynamic) static)
  ad1 = makeDesign(a1)
  addExperiments(reg, pd1, ad1)
  expect_error(addExperiments(reg, pd1, ad1), "identical experiments")

  # check adding of repls
  reg = makeTestRegistry()
  p1 = addProblem(reg, "p1", 1)
  pd1 = makeDesign(p1)
  a1 = addAlgorithm(reg, id="a1", fun=function(static, dynamic) static)
  ad1 = makeDesign(a1)
  addExperiments(reg, pd1, ad1, repls=2)
  expect_error(addExperiments(reg, pd1, ad1, repls=5), "identical experiments")
  addExperiments(reg, pd1, ad1, repls=5, skip.defined=TRUE)
  expect_equal(getJobNr(reg), 5)


  # check constraints
  reg = makeTestRegistry()
  p1 = addProblem(reg, "p1", 1)
  pd1 = makeDesign(p1)
  a1 = addAlgorithm(reg, id="a1", fun=function(static, dynamic, ...) static)
  ad1 = makeDesign(a1, exhaustive = list(x = 1:2, y = 5:6))
  addExperiments(reg, pd1, ad1, repls=2, skip.defined = TRUE)
  tab = BatchJobs:::dbGetJobStatusTable(reg)
  expect_true(all(! duplicated(tab$seed)))
  expect_true(all(! duplicated(tab$job_id)))
  expect_true(all(! duplicated(tab[c("job_def_id", "repl")])))
  tab = BatchJobs:::dbGetExpandedJobsTable(reg)
  dups = duplicated(tab$job_def_id)
  expect_equal(sum(dups), 4L)
  expect_equal(sum(duplicated(tab[c("prob_id", "algo_id", "prob_pars", "algo_pars")])), 4L)
  expect_true(all(! duplicated(tab[! dups, c("prob_id", "algo_id", "prob_pars", "algo_pars")])))


  # test skip.defined
  reg = makeTestRegistry()
  p1 = addProblem(reg, "p1", 1)
  pd1 = makeDesign(p1)
  a1 = addAlgorithm(reg, id="a1", fun=function(static, dynamic, ...) static)
  ad1 = makeDesign(a1, exhaustive = list(x = 1:2, y = 5:6))
  addExperiments(reg, pd1, ad1, repls=2)
  n1 = getJobNr(reg)
  expect_error(addExperiments(reg, pd1, ad1))
  addExperiments(reg, pd1, ad1, repls=2, skip.defined=TRUE)
  n2 = getJobNr(reg)
  expect_equal(n1, n2)
  addExperiments(reg, pd1, ad1, repls=3, skip.defined=TRUE)
  n3 = getJobNr(reg)
  expect_equal(n3, as.integer(n1 / 2 * 3))
})



