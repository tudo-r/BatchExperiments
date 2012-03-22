#' Generates a data.frame with one row per job id. The columns are: ids of problem and algorithm 
#' (prob.id and algo.id), one column per parameter of problem or algorithm (named by the parameter name),
#' the replication number (named repl) and all columns defined in the function to collect the values.
#' Note that you cannot rely on the order of the columns.
#' If a paramater does not have a setting for a certain job / experiment it is set to \code{NA}.  
#' @title Reduce results into a data.frame.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of selected experiments.
#'   Default is all jobs for which results are available.
#' @param part [\code{character(1)}]
#'   Only useful for multiple result files, then defines which result file part should be loaded.
#'   \code{NA} means all parts are loaded, which is the default.
#' @param fun [\code{function(job, res)}]\cr
#'   Function to collect values from \code{job} and result \code{res} object, the latter from stored result file.
#'   Must return a list which can be coerced to a data frame.
#' @param ... [any]\cr
#'   Additional arguments to \code{fun}.
#' @param strings.as.factors [\code{logical(1)}]
#'   Should all character columns in result be converted to factors?
#'   Default is \code{default.stringsAsFactors()}.
#' @param block.size [\code{integer(1)}]
#'   Results will be fetched in blocks of size \code{block.size}.
#'   Default is 100.
#' @return [\code{data.frame}] Aggregated results, containing problem and algorithm paramaters and collected values.
#' @export
reduceResultsSimple = function(reg, ids, part=as.character(NA), fun, ..., 
  strings.as.factors=default.stringsAsFactors(), block.size=100L) {
  
  checkArg(reg, cl = "ExperimentRegistry")
  checkArg(fun, formals=c("job", "res"))
  if (missing(ids)) {
    ids = findDone(reg)
  } else {
    ids = convertIntegers(ids)
    checkArg(ids, "integer", na.ok=FALSE)
    ids = intersect(ids, findDone(reg))
    BatchJobs:::checkIds(reg, ids)
  }
  block.size = convertInteger(block.size)
  checkArg(block.size, "integer", na.ok=FALSE, len=1L)

  n = length(ids)
  if (n == 0L)
    stop("No jobs with corresponding ids finished (yet).")
  messagef("Reducing %i results...", n)
  
 getRow = function(j, reg, part, ...) {
    c(list(prob = j$prob.id),
      j$prob.pars,
      list(algo = j$algo.id),
      j$algo.pars,
      list(repl = j$repl),
      fun(j, loadResult(reg, j$id, part, check.id=FALSE), ...))
  }
  
  aggr = data.frame()
  ids = chunk(ids, chunk.size=block.size)
  bar = makeProgressBar(max=length(ids), label="reduceResultsSimple")
  bar(0)

  for(i in seq_along(ids)) {
    jobs = getJobs(reg, ids[[i]], check.ids=FALSE)
    results = lapply(jobs, getRow, reg = reg, part = part, ...)
    aggr = rbind.fill(c(list(aggr), lapply(results, as.data.frame, stringsAsFactors=FALSE)))
    bar(i)
  }
  # FIXME is this bugged or why do we need this here?
  # remove FIXME with comment!
  if (strings.as.factors) {
    inds = which(sapply(aggr, is.character))
    for (j in inds)
      aggr[,j] = as.factor(aggr[,j])
  }
  return(aggr)
}
