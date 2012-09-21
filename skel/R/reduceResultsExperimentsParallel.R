#' Reduce very many results in parallel.
#'
#' Basically the same as \code{\link{reduceResultsExperiments}} but creates a few (hopefully short) jobs
#' to reduce the results in parallel. The function internally calls \code{\link{batchMapQuick}},
#' does \dQuote{busy-waiting} till
#' all jobs are done and cleans all temporary files up.
#' Useful when you have very results and reducing is slow.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of selected experiments.
#'   Default is all jobs for which results are available.
#' @param part [\code{character}]
#'   Only useful for multiple result files, then defines which result file part(s) should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param fun [\code{function(job, res, ...)}]\cr
#'   Function to collect values from \code{job} and result \code{res} object, the latter from stored result file.
#'   Must return an object which can be coerced to a \code{data.frame} (e.g. a \code{list}).
#'   Default is a function that simply returns \code{res} which may or may not work, depending on the type
#'   of \code{res}.
#' @param ... [any]\cr
#'   Additional arguments to \code{fun}.
#' @param njobs [\code{integer(1)}]
#'   Number of parallel jobs to create.
#'   Default is 20.
#' @param strings.as.factors [\code{logical(1)}]
#'   Should all character columns in result be converted to factors?
#'   Default is \code{default.stringsAsFactors()}.
#' @return [\code{data.frame}]. Aggregated results, containing problem and algorithm paramaters and collected values.
#' @export
reduceResultsExperimentsParallel = function(reg, ids, part=as.character(NA), fun, ..., njobs=20L, strings.as.factors=default.stringsAsFactors()) {
  checkArg(reg, "ExperimentRegistry")
  done = BatchJobs:::dbGetDone(reg)
  if (missing(ids)) {
    ids = done
  } else {
    ids = BatchJobs:::checkIds(reg, ids)
    if (any(ids %nin% done))
      stopf("No results available for experiments with ids: %s", collapse(ids[ids %nin% done]))
  }
  BatchJobs:::checkPart(reg, part)
  if (missing(fun)){
    fun = function(job, res) res
  } else {
    force(fun)
    checkArg(fun, formals=c("job", "res"))
  }
  njobs = convertInteger(njobs)
  checkArg(njobs, "integer", len=1L, na.ok=FALSE)
  checkArg(strings.as.factors, "logical", len=1L, na.ok=FALSE)

  n = length(ids)
  messagef("Reducing %i results...", n)

  ch = chunk(ids, n.chunks=njobs, shuffle=FALSE)
  more.args = c(list(reg=reg, part=part, fun=fun, strings.as.factors=strings.as.factors), list(...))
  # FIXME: Magic constant 10
  # FIXME: file.dir of reg2 should point to subdir of reg$file.dir
  #        m/b provide option
  reg2 = batchMapQuick(function(reg, ii, fun, part, strings.as.factors, ...) {
    reduceResultsExperiments(reg, ii, part=part, fun=fun,
      block.size=ceiling(length(ii) / 10), strings.as.factors=strings.as.factors, ...)
  }, ch, more.args=more.args)
  while (length(findMissingResults(reg2)) > 0L) {
    # FIXME: what happens if jobs hit the wall time?
    #        infinite loop?
    errids = findErrors(reg2)
    if (length(errids) > 0L) {
      # FIXME: wrong db fun
      df = BatchJobs:::dbGetJobStatusTable(reg2, errids[1L])
      stopf("There were some errors like: %s", df$error[1L])
    }
    Sys.sleep(10)
  }

  res = reduceResults(reg2, fun=function(aggr, job, res) {
    d = rbind.fill(aggr, res)
    attr(d, "prob.pars.names") = union(attr(aggr, "prob.pars.names"), attr(res, "prob.pars.names"))
    attr(d, "algo.pars.names") = union(attr(aggr, "algo.pars.names"), attr(res, "algo.pars.names"))
    return(d)
  }, init=data.frame())
  class(res) = c("ReducedResultsExperiments", class(res))
  if (nrow(res) == 0L) {
    attr(res, "prob.pars.names") = character(0L)
    attr(res, "algo.pars.names") = character(0L)
  }
  unlink(reg2$file.dir, recursive=TRUE)
  return(res)
}

