#' Group experiments.
#'
#' Creates a list of \code{\link{factor}} to use in functions like \code{\link{tapply}}, \code{\link{by}}
#' or \code{\link{aggregate}}.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   If not missing, restict grouping to this subset of experiment ids.
#' @param by.prob [\code{logical}]\cr
#'   Group experiments by problem. Default is \code{FALSE}.
#' @param by.algo [\code{logical}]\cr
#'   Group experiments by algorithm. Default is \code{FALSE}.
#' @param by.repl [\code{logical}]\cr
#'   Group experiments by replication. Default is \code{FALSE}.
#' @param by.prob.pars [quoted R expression]\cr
#'   If not missing, group experiments by this evaluated R expression.
#'   The expression is evaluated in the environment of problem parameters and
#'   converted to a factor using \code{as.factor}.
#' @param by.algo.pars [quoted R expression]\cr
#'   If not missing, group experiments by this evaluated R expression.
#'   The expression is evaluated in the environment of algorithm parameters and
#'   converted to a factor using \code{\link{as.factor}}.
#' @return [\code{list}]. List of factors.
#' @examples
#' # create a registry and add problems and algorithms
#' reg = makeExperimentRegistry("getIndex", file.dir=tempfile(""))
#' addProblem(reg, "prob", static = 1)
#' addAlgorithm(reg, "f0", function(static, dynamic) static)
#' addAlgorithm(reg, "f1", function(static, dynamic, i, k) static * i^k)
#' ad = list(makeDesign("f0"), makeDesign("f1", exhaustive=list(i=1:10, k=1:3)))
#' addExperiments(reg, algo.designs=ad)
#' submitJobs(reg)
#'
#' # get grouped job ids
#' ids = getJobIds(reg)
#' by(ids, getIndex(reg, by.prob=TRUE, by.algo=TRUE), identity)
#' ids.f1 = findExperiments(reg, algo.pattern="f1")
#' by(ids.f1, getIndex(reg, ids.f1, by.algo.pars=quote(k == 1)), identity)
#'
#' # groupwise reduction
#' ids.f1 = findExperiments(reg, algo.pattern="f1")
#' f = function(aggr, job, res) aggr + res
#' by(ids.f1, getIndex(reg, ids.f1, by.algo.pars=quote(k)), reduceResults, reg=reg, fun=f)
#' by(ids.f1, getIndex(reg, ids.f1, by.algo.pars=quote(i)), reduceResults, reg=reg, fun=f)
#' @export
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

  if (missing(by.prob.pars) && missing(by.algo.pars)) {
    # if not dealing with parameters, we can get the groups directly
    # from the database
    cols = c("job_id", "prob_id", "algo_id", "repl")[c(TRUE, by.prob, by.algo, by.repl)]
    query = sprintf("SELECT %s FROM %s_expanded_jobs", collapse(cols), reg$id)
    index = BatchJobs:::dbSelectWithIds(reg, query, ids)[, -1L, drop=FALSE]
    names(index) = gsub("_id", "", names(index), fixed=TRUE)
  } else {
    # otherwise we have to get all jobs and calculate the groups on them
    jobs = getJobs(reg, ids, check.ids=FALSE)
    index = list()

    exprToIndex = function(jobs, expr, name) {
      str.expr = capture.output(print(expr))
      evaluated = try(lapply(jobs, function(j) eval(expr, j[[name]])), silent=TRUE)
      if (is.error(evaluated))
        stopf("Your %s expression resulted in an error:\n%s", name, as.character(evaluated))
      evaluated = try(as.factor(unlist(evaluated)))
      if (is.error(evaluated) || length(evaluated) != length(jobs))
        stopf("The return value of expression %s ('%s') is not convertible to a factor", name, str.expr)
      namedList(sprintf("algo.pars: %s", str.expr), evaluated)
    }

    if (by.prob)
      index = c(index, list(prob = extractSubList(jobs, "prob.id", character(1L))))
    if (by.algo)
      index = c(index, list(algo = extractSubList(jobs, "algo.id", character(1L))))
    if (by.repl)
      index = c(index, list(repl = extractSubList(jobs, "repl", integer(1L))))

    if (!missing(by.prob.pars)) {
      if (!BatchJobs:::is.evaluable(by.prob.pars))
        stop("Argument by.prob.pars must be a call, expression or symbol")
      index = c(index, exprToIndex(jobs, by.prob.pars, "prob.pars"))
    }
    if (!missing(by.algo.pars)) {
      if (!BatchJobs:::is.evaluable(by.algo.pars))
        stop("Argument by.algo.pars must be a call, expression or symbol")
      index = c(index, exprToIndex(jobs, by.algo.pars, "algo.pars"))
    }
  }

  lapply(index, as.factor)
}
