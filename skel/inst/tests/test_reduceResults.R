context("reduceResults")

test_that("reduceResults", {
  reg = makeTestRegistry()
  addProblem(reg, "p1", static=1)
  addProblem(reg, "p2", static=2)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic) static*1)
  addAlgorithm(reg, id="a2", fun=function(static, dynamic) static*2)
  addExperiments(reg, c("p1", "p2"), c("a1", "a2"), 2)
  submitJobs(reg)
  data = reduceResults(reg, fun=function(aggr, job, res)
    rbind(aggr, data.frame(prob=job$prob.id, algo=job$algo.id,
      repl=job$repl, y=res, stringsAsFactors = FALSE)),
    init = data.frame()
  )
  data2 = data.frame(
    prob = c("p1", "p1", "p1", "p1", "p2", "p2", "p2", "p2"),
    algo = c("a1", "a1", "a2", "a2", "a1", "a1", "a2", "a2"),
    repl = as.integer(c(1,2,1,2,1,2,1,2)),
    y = c(1,1,2,2,2,2,4,4),
    stringsAsFactors = FALSE
  )
  expect_equal(data, data2)
  data = reduceResultsExperiments(reg, fun=function(job, res) data.frame(y=res), strings.as.factors=FALSE)
  expect_equal(data, data2)

  reg = makeTestRegistry()
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic, a) 1*static)
  addAlgorithm(reg, id="a2", fun=function(static, dynamic, a, b) 2*static)
  ad1 = makeDesign("a1", exhaustive=list(a=1:2))
  ad2 = makeDesign("a2", exhaustive=list(a=3, b=c("b1", "b2")))
  addExperiments(reg, "p1", list(ad1, ad2))
  submitJobs(reg)
  data = reduceResultsExperiments(reg, fun=function(job, res) data.frame(y=res), strings.as.factors=TRUE)
  expect_equal(data, data.frame(
    prob = c("p1", "p1", "p1", "p1"),
    algo = c("a1", "a1", "a2", "a2"),
    a = c(1,2,3,3),
    repl = rep(1L, 4),
    y = c(1,1,2,2),
    b = c(NA,NA,"b1","b2"),
    stringsAsFactors = TRUE
  ))

  reg = makeTestRegistry()
  reg$seed = 1
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic, a) 1*static)
  addExperiments(reg, "p1", ad1)
  submitJobs(reg)
  data = reduceResultsExperiments(reg, fun=function(job, res) data.frame(y=res, seed=job$seed),
    strings.as.factors=FALSE)
  expect_equal(data, data.frame(
    prob = c("p1", "p1"),
    algo = c("a1", "a1"),
    a = c(1,2),
    repl = rep(1L, 2),
    y = c(1,1),
    seed = 1:2,
    stringsAsFactors = FALSE
  ))
})

test_that("reduceResultsExperiments works on empty id sets", {
  reg = makeTestRegistry()
  reg$seed = 1
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic, a) 1*static)
  addExperiments(reg, repls=10)
  expect_equal(reduceResultsExperiments(reg, fun=function(job, res) data.frame(y=res, seed=job$seed), strings.as.factors=FALSE), data.frame())
  submitJobs(reg)
  expect_equal(reduceResultsExperiments(reg, fun=function(job, res) data.frame(y=res, seed=job$seed), strings.as.factors=FALSE, ids = integer(0)), data.frame())

})
test_that("params are available in reduceResults", {
  reg = makeTestRegistry()
  addProblem(reg, "p1", dynamic=function(static, alpha) alpha)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic, beta) dynamic*beta)
  pd1 = makeDesign("p1", exhaustive=list(alpha=1:2))
  ad1 = makeDesign("a1", exhaustive=list(beta=3:4))
  addExperiments(reg, pd1, ad1)
  submitJobs(reg)
  data = reduceResults(reg, fun=function(aggr, job, res) {
    rbind(aggr, data.frame(alpha=job$prob.pars$alpha, beta=job$algo.pars$beta, y=res))
  }, init=data.frame())
  expect_equal(data, data.frame(
    alpha = c(1, 1, 2, 2),
    beta = c(3, 4, 3, 4),
    y = c(3, 4, 6, 8)
  ))
})



test_that("getProblem is available in reduceResults", {
  reg = makeTestRegistry()
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic) 2)
  addExperiments(reg, "p1", "a1", repls=2)
  submitJobs(reg)
  z = reduceResults(reg, fun=function(aggr, job, res) {
    pid = job$prob.id
    p = getProblem(reg, pid)
    static = p$static
    list(pid, static, res)
  })
  expect_equal(z, list("p1", 1, 2))
})



test_that("reduceResultsExperiments works with default fun", {
  reg = makeTestRegistry()
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic) list(y=1))
  addExperiments(reg, "p1", "a1", repls=2)
  submitJobs(reg)
  z = reduceResultsExperiments(reg)
  expect_equal(z, data.frame(
    prob = "p1",
    algo = "a1",
    repl = 1:2,
    y = 1))

  reg = makeTestRegistry()
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic) 1)
  addExperiments(reg, "p1", "a1", repls=2)
  submitJobs(reg)
  z = reduceResultsExperiments(reg)
  expect_equal(z, data.frame(
    prob = "p1",
    algo = "a1",
    repl = 1:2,
    X1 = 1))

  reg = makeTestRegistry()
  addProblem(reg, "p1", static=1)
  addAlgorithm(reg, id="a1", fun=function(static, dynamic) c(foo=1, bar=2))
  addExperiments(reg, "p1", "a1", repls=2)
  submitJobs(reg)
  z = reduceResultsExperiments(reg)
  expect_equal(z, data.frame(
    prob = "p1",
    algo = "a1",
    repl = 1:2,
    foo = 1,
    bar = 2))
})


