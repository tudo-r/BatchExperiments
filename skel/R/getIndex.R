# We could provide a by-option in reduceResults[.*] or let the user
# himself use these indicies (see examples below). Still unsure
# about this.
# Another possibility would be to provide "by.ExperimentRegistry"
#
# this is not exported right now.
getIndex = function(reg, ids, by.prob=FALSE, by.algo=FALSE, by.repl=FALSE,
                         by.prob.pars, by.algo.pars) {
  checkArg(reg, "ExperimentRegistry")
  if (!missing(ids)) {
    ids = convertIntegers(ids)
    checkArg(ids, "integer", na.ok=FALSE)
    BatchJobs:::checkIds(reg, ids)
  }
  checkArg(by.prob, "logical", na.ok=FALSE, len=1L)
  checkArg(by.algo, "logical", na.ok=FALSE, len=1L)
  checkArg(by.repl, "logical", na.ok=FALSE, len=1L)

  # if not dealing with parameters, we can get the groups directly
  # from the database
  if (missing(by.prob.pars) && missing(by.algo.pars)) {
    cols = c("job_id", "prob_id", "algo_id", "repl")[c(TRUE, by.prob, by.algo, by.repl)]
    query = sprintf("SELECT %s FROM %s_expanded_jobs", collapse(cols), reg$id)
    res = BatchJobs:::dbSelectWithIds(reg, query, ids)[, -1L, drop=FALSE]
    return(lapply(res, as.factor))
  }

  # otherwise we have to get all jobs and calculate the groups on them
  jobs = getJobs(reg, ids, check.ids=FALSE)
  index = list()

  # small helper function which evaluates expr in a data.frame
  exprToIndex = function(jobs, expr, name) {
    # FIXME is the evaluation in the data.frame problematic?
    pars = rbind.fill(lapply(jobs, function(x) as.data.frame(x[[name]])))
    evaluated = with(pars, eval(expr))
    namedList(sprintf("algo.pars: %s", capture.output(print(expr))), evaluated)
  }

  if (by.prob)
    index = c(index, list(prob = extractSubList(jobs, "prob.id", character(1L))))
  if (by.algo)
    index = c(index, list(algo = extractSubList(jobs, "algo.id", character(1L))))
  if (by.repl)
    index = c(index, list(repl = extractSubList(jobs, "repl", integer(1L))))

  if (!missing(by.prob.pars)) {
    if (! (is.call(by.prob.pars) || is.expression(by.prob.pars)))
      stop("Argument by.prob.pars must be a call or expression")
    index = c(index, exprToIndex(jobs, by.prob.pars, "prob.pars"))
  }
  if (!missing(by.algo.pars)) {
    if (! (is.call(by.algo.pars) || is.expression(by.algo.pars)))
      stop("Argument by.algo.pars must be a call or expression")
    index = c(index, exprToIndex(jobs, by.algo.pars, "algo.pars"))
  }

  lapply(index, as.factor)
}

# EXAMPLES:
# library(BatchExperiments)
# reg = makeExperimentRegistry("foo", file.dir=tempfile())
# addProblem(reg, "iris", static = iris)
# addAlgorithm(reg, "1", function(static, dynamic, i) i)
# addAlgorithm(reg, "2", function(static, dynamic) 2)
# addAlgorithm(reg, "3", function(static, dynamic) 3)
# ad = list(makeDesign("1", exhaustive=list(i=1:10)), makeDesign("2"), makeDesign("3"))
# addExperiments(reg, algo.designs=ad, repls=2)
# submitJobs(reg)
# # get grouped job ids (this might get a helper fun)
# by(getJobIds(reg), getIndex(reg, by.prob=TRUE, by.algo=TRUE), identity)
# # groupwise reduction
# f = function(aggr, job, res) aggr + res
# by(getJobIds(reg), getIndex(reg, by.algo=TRUE), reduceResults, reg=reg, fun=f)
# ids = findExperiments(reg, algo.pattern = "1")
# by(ids, getIndex(reg, ids, by.algo.pars=quote(i > 5)), reduceResults, reg=reg, fun=f)
