#' Find ids of experiments that match a query.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param prob.pattern [\code{character(1)}]\cr
#'   If not missing, all problem ids that match this string are selected.
#' @param prob.pars [R expression]\cr
#'   If not missing, all problems whose parameters match
#'   the given expression are selected.
#' @param algo.pattern [\code{character(1)}]\cr
#'   If not missing, all algorithm ids that match this string are selected.
#' @param algo.pars [R expression]\cr
#'   If not missing, all algorithms whose parameters match
#'   the given expression are selected.
#' @param repls [\code{integer}]\cr
#'   If not missing, restrict to jobs with given replication numbers.
#' @param match.substring [\code{logical(1)}]\cr
#'   Is a match in \code{prob.pattern} and \code{algo.pattern} if the id contains
#'   the pattern as substring or must the id exactly match?
#'   Default is \code{TRUE}.
#' @return [\code{integer}]. Ids for experiments which match the query.
#' @export
findExperiments = function(reg, prob.pattern, prob.pars, algo.pattern, algo.pars, repls, match.substring=TRUE) {
  checkArg(reg, cl="ExperimentRegistry")
  if (!missing(prob.pattern))
    checkArg(prob.pattern, "character", len=1L, na.ok=FALSE)
  if (!missing(algo.pattern))
    checkArg(algo.pattern, "character", len=1L, na.ok=FALSE)
  if (!missing(repls)) {
    repls = convertIntegers(repls)
    checkArg(repls, "integer", lower=1L, na.ok=FALSE)
  }

  ids = dbFindExperiments(reg, prob.pattern, algo.pattern, repls, like=match.substring)

  # skip possible expensive furhter calculations if possible
  if (length(ids) == 0L || (missing(prob.pars) && missing(algo.pars)))
    return(ids)

  jobs = getJobs(reg, ids, check.ids=FALSE)

  if (!missing(prob.pars)) {
    ind = vapply(jobs, function(job, pars, ee) eval(pars, job$prob.pars, ee),
                 logical(1L), pars=substitute(prob.pars), ee=parent.frame())
    jobs = jobs[!is.na(ind) & ind]
  }
  if (!missing(algo.pars)) {
    ind = vapply(jobs, function(job, pars, ee) eval(pars, job$algo.pars, ee),
                 logical(1L), pars=substitute(algo.pars), ee=parent.frame())
    jobs = jobs[!is.na(ind) & ind]
  }
  return(extractSubList(jobs, "id", element.value=integer(1L)))
}
