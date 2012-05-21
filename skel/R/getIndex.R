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
#' @param by.prob.pars [R expression]\cr
#'   If not missing, group experiments by this R expression.
#'   The expression is evaluated in the environment of problem parameters and
#'   converted to a factor using \code{as.factor}.
#' @param by.algo.pars [R expression]\cr
#'   If not missing, group experiments by this R expression.
#'   The expression is evaluated in the environment of algorithm parameters and
#'   converted to a factor using \code{\link{as.factor}}.
#' @return [\code{list}]. List of factors.
#' @export
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
#' by(ids.f1, getIndex(reg, ids.f1, by.algo.pars=(k == 1)), identity)
#'
#' # groupwise reduction
#' ids.f1 = findExperiments(reg, algo.pattern="f1")
#' f = function(aggr, job, res) aggr + res
#' by(ids.f1, getIndex(reg, ids.f1, by.algo.pars=k), reduceResults, reg=reg, fun=f)
#' by(ids.f1, getIndex(reg, ids.f1, by.algo.pars=i), reduceResults, reg=reg, fun=f)
getIndex = function(reg, ids, by.prob=FALSE, by.algo=FALSE, by.repl=FALSE,
                    by.prob.pars, by.algo.pars) {
  checkArg(reg, "ExperimentRegistry")
  if (!missing(ids))
    ids = BatchJobs:::checkIds(reg, ids)
  checkArg(by.prob, "logical", na.ok=FALSE, len=1L)
  checkArg(by.algo, "logical", na.ok=FALSE, len=1L)
  checkArg(by.repl, "logical", na.ok=FALSE, len=1L)

  if (missing(by.prob.pars) && missing(by.algo.pars)) {
    # if not dealing with parameters, we can get the groups directly
    # from the database
    cols = c("job_id", "prob_id", "algo_id", "repl")[c(TRUE, by.prob, by.algo, by.repl)]
    query = sprintf("SELECT %s FROM %s_expanded_jobs", collapse(cols), reg$id)
    index = BatchJobs:::dbSelectWithIds(reg, query, ids)[, -1L, drop=FALSE]
    names(index) = c("prob", "algo", "repl")[c(by.prob, by.algo, by.repl)]
  } else {
    # otherwise we have to get all jobs and calculate the groups on them
    exprToIndex = function(jobs, pars, ee, name) {
      ind = try(lapply(jobs, function(job, pars, ee, name) eval(pars, job[[name]], ee),
                       pars = pars, ee = ee, name=name), silent=TRUE)
      if (is.error(ind))
        stopf("Your %s expression resulted in an error:\n%s", name, as.character(ind))
      ind = try(as.factor(unlist(ind)))
      str.expr = capture.output(print(pars))
      if (is.error(ind) || length(ind) != length(jobs))
        stopf("The return value of expression %s ('%s') is not convertible to a factor", name, str.expr)
      namedList(sprintf("%s: %s", name, str.expr), ind)
    }

    jobs = getJobs(reg, ids, check.ids=FALSE)
    index = list()

    if (by.prob)
      index = c(index, list(prob = extractSubList(jobs, "prob.id", character(1L))))
    if (by.algo)
      index = c(index, list(algo = extractSubList(jobs, "algo.id", character(1L))))
    if (by.repl)
      index = c(index, list(repl = extractSubList(jobs, "repl", integer(1L))))
    if (!missing(by.prob.pars))
      index = c(index, exprToIndex(jobs, substitute(by.prob.pars), parent.frame(), "prob.pars"))
    if (!missing(by.algo.pars))
      index = c(index, exprToIndex(jobs, substitute(by.algo.pars), parent.frame(), "algo.pars"))
  }

  lapply(index, as.factor)
}
