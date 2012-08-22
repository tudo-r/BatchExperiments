context("reduceResultsExperimentsParallel")

test_that("reduceResultsExperimentsParallel", {
  njobs=3
  reg = makeTestRegistry()
  addProblem(reg, "p1", static=1)
  addProblem(reg, "p2", static=2)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic) static*1)
  addAlgorithm(reg, id="a2", fun=function(static, dynamic) static*2)
  addExperiments(reg, c("p1", "p2"), c("a1", "a2"), 2)
  submitJobs(reg)
  data = reduceResultsExperimentsParallel(reg, fun=function(job, res) data.frame(y=res), strings.as.factors=FALSE, njobs=njobs)
  data2 = data.frame(
    prob = c("p1", "p1", "p1", "p1", "p2", "p2", "p2", "p2"),
    algo = c("a1", "a1", "a2", "a2", "a1", "a1", "a2", "a2"),
    repl = as.integer(c(1,2,1,2,1,2,1,2)),
    y = c(1,1,2,2,2,2,4,4),
    stringsAsFactors = FALSE
  )
  attr(data2, "prob.pars.names") = character(0)
  attr(data2, "algo.pars.names") = character(0)
  class(data2) = c("ReducedResultsExperiments", "data.frame")
  expect_equal(data, data2)
  
  reg = makeTestRegistry()
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic, a) 1*static)
  addAlgorithm(reg, id="a2", fun=function(static, dynamic, a, b) 2*static)
  ad1 = makeDesign("a1", exhaustive=list(a=1:2))
  ad2 = makeDesign("a2", exhaustive=list(a=3, b=c("b1", "b2")))
  addExperiments(reg, "p1", list(ad1, ad2))
  submitJobs(reg)
  data = reduceResultsExperimentsParallel(reg, fun=function(job, res) data.frame(y=res), strings.as.factors=TRUE)
  data2 = data.frame(
    prob = c("p1", "p1", "p1", "p1"),
    algo = c("a1", "a1", "a2", "a2"),
    a = c(1,2,3,3),
    repl = rep(1L, 4),
    y = c(1,1,2,2),
    b = c(NA,NA,"b1","b2"),
    stringsAsFactors = TRUE)  
  attr(data2, "prob.pars.names") = character(0)
  attr(data2, "algo.pars.names") = c("a", "b")
  class(data2) = c("ReducedResultsExperiments", "data.frame")
  expect_equal(getResultVars(data, "prob.pars"), character(0))
  expect_equal(getResultVars(data, "algo.pars"), c("a", "b"))
  expect_equal(getResultVars(data, "group"), c("prob", "algo", "a", "b"))
  expect_equal(getResultVars(data, "result"), "y")
  
  reg = makeTestRegistry()
  reg$seed = 1
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic, a) 1*static)
  addExperiments(reg, "p1", ad1)
  submitJobs(reg)
  data = reduceResultsExperiments(reg, fun=function(job, res) data.frame(y=res, seed=job$seed),
                                  strings.as.factors=FALSE)
  data2 = data.frame(
    prob = c("p1", "p1"),
    algo = c("a1", "a1"),
    a = c(1,2),
    repl = rep(1L, 2),
    y = c(1,1),
    seed = 1:2,
    stringsAsFactors = FALSE)
  attr(data2, "prob.pars.names") = character(0)
  attr(data2, "algo.pars.names") = c("a")
  class(data2) = c("ReducedResultsExperiments", "data.frame")
  expect_equal(data, data2)
})

test_that("reduceResultsExperimentsParallel works on empty id sets", {
  reg = makeTestRegistry()
  reg$seed = 1
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic, a) 1*static)
  addExperiments(reg, repls=10)
  data2 = data.frame()
  attr(data2, "prob.pars.names") = character(0)
  attr(data2, "algo.pars.names") = character(0)
  class(data2) = c("ReducedResultsExperiments", "data.frame")
  expect_equal(
    reduceResultsExperimentsParallel(reg, fun=function(job, res) data.frame(y=res, seed=job$seed), 
                             strings.as.factors=FALSE),
    data2)
  submitJobs(reg)
  expect_equal(
    reduceResultsExperimentsParallel(reg, fun=function(job, res) data.frame(y=res, seed=job$seed), 
                             strings.as.factors=FALSE, ids = integer(0)), 
    data2)
})


test_that("reduceResultsExperimentsParallel works with default fun", {
  reg = makeTestRegistry()
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic) list(y=1))
  addExperiments(reg, "p1", "a1", repls=2)
  submitJobs(reg)
  z = reduceResultsExperimentsParallel(reg)
  expect_equal(z, data.frame(
    prob = "p1",
    algo = "a1",
    repl = 1:2,
    y = 1), check.attributes=FALSE)
  
  reg = makeTestRegistry()
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic) 1)
  addExperiments(reg, "p1", "a1", repls=2)
  submitJobs(reg)
  z = reduceResultsExperimentsParallel(reg)
  expect_equal(z, data.frame(
    prob = "p1",
    algo = "a1",
    repl = 1:2,
    X1 = 1), check.attributes=FALSE)
  
  reg = makeTestRegistry()
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic) c(foo=1, bar=2))
  addExperiments(reg, "p1", "a1", repls=2)
  submitJobs(reg)
  z = reduceResultsExperimentsParallel(reg)
  expect_equal(z, data.frame(
    prob = "p1",
    algo = "a1",
    repl = 1:2,
    foo = 1,
    bar = 2), check.attributes=FALSE)
})

