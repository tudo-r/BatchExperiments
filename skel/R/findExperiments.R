#' Subset experiment ids from the registry
#'
#' Finds ids of experiments that match a query.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param prob.pattern [\code{character(1)}]\cr
#'   If not missing, all problem ids that match this substring are selected.
#' @param prob.pars [quoted R expression]\cr
#'   If not missing, all problems whose selected parameters match 
#'   the given expression are selected.
#' @param algo.pattern [\code{character(1)}]\cr
#'   If not missing, all algorithm ids that match this substring are selected.
#' @param algo.pars [quoted R expression]\cr
#'   If not missing, all algorithms whose selected parameters match 
#'   the given expression are selected.
#' @param repls [\code{integer}]\cr
#'   If not missing, restrict to jobs with given replication numbers.
#' @return [\code{integer}]. Ids for experiments which match the query.
#' @export
findExperiments = function(reg, prob.pattern, prob.pars, algo.pattern, algo.pars, repls) {
  checkArg(reg, cl="ExperimentRegistry")
  if (!missing(repls)) {
    repls = convertIntegers(repls)
    checkArg(repls, "integer", lower=1L, na.ok=TRUE)
  }
  if (getJobNr(reg) == 0L)
    return(integer(0L))

  ids = dbFindExperiments(reg, prob.pattern, algo.pattern, repls, like=TRUE)
  if (missing(prob.pars) && missing(algo.pars))
    return(ids)

  jobs = getJobs(reg, ids, check.ids=FALSE)
  if (!missing(prob.pars)) {
    jobs = Filter(function(j) eval(prob.pars, j$prob.pars), jobs)
  }
  if (!missing(algo.pars)) {
    jobs = Filter(function(j) eval(algo.pars, j$algo.pars), jobs)
  }
  return(extractSubList(jobs, "id", element.value=integer(1L)))
}
