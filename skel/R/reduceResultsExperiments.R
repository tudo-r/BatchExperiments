#' Reduce results into a data.frame with all relevant information.
#'
#' Generates a \code{data.frame} with one row per job id. The columns are: ids of problem and algorithm
#' (named \dQuote{prob} and \dQuote{algo}), one column per parameter of problem or algorithm (named by the parameter name),
#' the replication number (named \dQuote{repl}) and all columns defined in the function to collect the values.
#' Note that you cannot rely on the order of the columns.
#' If a paramater does not have a setting for a certain job / experiment it is set to \code{NA}.
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
#' @param strings.as.factors [\code{logical(1)}]
#'   Should all character columns in result be converted to factors?
#'   Default is \code{default.stringsAsFactors()}.
#' @param block.size [\code{integer(1)}]
#'   Results will be fetched in blocks of size \code{block.size}.
#'   Default is 100.
#' @return [\code{data.frame}] Aggregated results, containing problem and algorithm paramaters and collected values.
#' @export
reduceResultsExperiments = function(reg, ids, part=as.character(NA), fun, ...,
  strings.as.factors=default.stringsAsFactors(), block.size=100L) {

  checkArg(reg, cl = "ExperimentRegistry")
  done = BatchJobs:::dbGetDone(reg)
  if (missing(ids)) {
    ids = done
  } else {
    ids = BatchJobs:::checkIds(reg, ids)
    if (!all(ids %in% done))
      stopf("No results available for experiments with ids: %s", collapse(ids[!(ids %in% done)]))
  }
  BatchJobs:::checkPart(reg, part)  
  if (missing(fun)){
    fun = function(job, res) res
  } else {
    force(fun)
    checkArg(fun, formals=c("job", "res"))
  }
  checkArg(strings.as.factors, "logical", len=1L, na.ok=FALSE)
  block.size = convertInteger(block.size)
  checkArg(block.size, "integer", len=1L, na.ok=FALSE)
  
  n = length(ids)
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
  bar = makeProgressBar(max=length(ids), label="reduceResultsExperiments")
  bar$set()
  
  tryCatch({
    for(id.chunk in ids) {
      jobs = getJobs(reg, id.chunk, check.ids=FALSE)
      results = lapply(jobs, getRow, reg = reg, part = part, ...)
      aggr = rbind.fill(c(list(aggr), lapply(results, as.data.frame, stringsAsFactors=FALSE)))
      bar$inc(1L)
    }
  }, error=bar$error)

  if (strings.as.factors) {
    inds = which(vapply(aggr, is.character, logical(1L)))
    for (j in inds)
      aggr[,j] = as.factor(aggr[,j])
  }
  return(aggr)
}
