#' Reduce results into a data.frame with all relevant information.
#'
#' Generates a \code{data.frame} with one row per job id. The columns are: ids of problem and algorithm
#' (named \dQuote{prob} and \dQuote{algo}), one column per parameter of problem or algorithm (named by the parameter name),
#' the replication number (named \dQuote{repl}) and all columns defined in the function to collect the values.
#' Note that you cannot rely on the order of the columns.
#' If a paramater does not have a setting for a certain job / experiment it is set to \code{NA}.
#' Have a look at \code{\link{getResultVars}} if you want to use somethink like \code{\link{ddply}} on the
#' results.
#'
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
#' @param strings.as.factors [\code{logical(1)}]
#'   Should all character columns in result be converted to factors?
#'   Default is \code{default.stringsAsFactors()}.
#' @param block.size [\code{integer(1)}]
#'   Results will be fetched in blocks of this size.
#'   Default is max(100, 5 percent of ids).
#' @param impute.val [\code{named list}]\cr
#'   If not missing, the value of \code{impute.val} is used as a replacement for the
#'   return value of function \code{fun} on missing results. An empty list is allowed.
#' @return [\code{data.frame}]. Aggregated results, containing problem and algorithm paramaters and collected values.
#' @aliases ReducedResultsExperiments
#' @export
reduceResultsExperiments = function(reg, ids, part=NA_character_, fun, ...,
  strings.as.factors=default.stringsAsFactors(), block.size, impute.val) {

  checkExperimentRegistry(reg, strict=TRUE)
  BatchJobs:::syncRegistry(reg)
  if (missing(ids)) {
    ids = done = BatchJobs:::dbFindDone(reg)
    with.impute = FALSE
  } else {
    ids = BatchJobs:::checkIds(reg, ids)
    done = BatchJobs:::dbFindDone(reg, ids)
    with.impute = !missing(impute.val)
    if (with.impute) {
      if (!is.list(impute.val) || !isProperlyNamed(impute.val))
        stop("Argument 'impute.val' must be a properly named list")
    } else {
      if (length(ids) > length(done))
        stopf("No results available for jobs with ids: %s", collapse(setdiff(ids, done)))
    }
  }
  BatchJobs:::checkPart(reg, part)
  if (missing(fun))
    fun = function(job, res) res
  else
    checkArg(fun, formals=c("job", "res"))

  checkArg(strings.as.factors, "logical", len=1L, na.ok=FALSE)
  if (missing(block.size)) {
    block.size = max(100L, as.integer(0.05 * length(ids)))
  } else {
    block.size = convertInteger(block.size)
    checkArg(block.size, "integer", len=1L, na.ok=FALSE)
  }

  n = length(ids)
  messagef("Reducing %i results...", n)

  impute = function(job, res, ...)
    impute.val
  getRow = function(j, reg, part, .fun, ...)
    c(list(prob=j$prob.id), j$prob.pars, list(algo=j$algo.id), j$algo.pars, list(repl=j$repl),
      .fun(j, BatchJobs:::getResult(reg, j$id, part), ...))

  aggr = data.frame()
  ids2 = chunk(ids, chunk.size=block.size, shuffle=FALSE)
  bar = makeProgressBar(max=length(ids2), label="reduceResultsExperiments")
  bar$set()
  prob.pars = character(0L)
  algo.pars = character(0L)

  tryCatch({
    for(id.chunk in ids2) {
      # FIXME: getJobs is inefficient here, we just want a data.frame
      # FIXME: also check all other functions using getJobs / rbind.fill
      jobs = getJobs(reg, id.chunk, check.ids=FALSE)
      prob.pars = unique(c(prob.pars, unlist(lapply(jobs, function(j) names(j$prob.pars)))))
      algo.pars = unique(c(algo.pars, unlist(lapply(jobs, function(j) names(j$algo.pars)))))
      # FIXME m/b use list2df instead of rbind.fill
      # -> major problem: how to deal with missing names in return value of fun?
      #    rbind.fill might not do the right thing here, also.
      id.chunk.done = id.chunk %in% done
      results = c(lapply(jobs[ id.chunk.done], getRow, reg=reg, part=part, .fun=fun, ...),
                  lapply(jobs[!id.chunk.done], getRow, reg=reg, part=part, .fun=impute, ...))
      aggr = rbind.fill(c(list(aggr), lapply(results, as.data.frame, stringsAsFactors=FALSE)))
      bar$inc(1L)
    }
  }, error=bar$error)

  aggr = convertDataFrameCols(aggr, chars.as.factor=strings.as.factors)
  if (nrow(aggr))
    aggr = setRowNames(cbind(id=ids, aggr), ids)
  aggr = addClasses(aggr, "ReducedResultsExperiments")
  attr(aggr, "prob.pars.names") = prob.pars
  attr(aggr, "algo.pars.names") = algo.pars
  return(aggr)
}

#' Get variable groups of reduced results.
#'
#' Useful helper for e.g. package plyr and such.
#'
#' @param data [\code{\link{ReducedResultsExperiments}}]\cr
#'   Result data.frame from \code{\link{reduceResultsExperiments}}.
#' @param type [\code{character(1)}]\cr
#'   Can be \dQuote{prob} (prob + pars), \dQuote{prob.pars} (only problem pars),
#'   \dQuote{algo} (algo + pars), \dQuote{algo.pars} (only algo pars),
#'   \dQuote{group} (prob + problem pars + algo + algo pars), \dQuote{result} (result column names).
#'   Default is \dQuote{group}.
#' @return [\code{character}]. Names of of columns.
#' @export
#' @examples
#' \dontrun{
#' reg <- makeExperimentRegistry("BatchExample", seed=123, file.dir=tempfile())
#' addProblem(reg, "p1", static=1)
#' addProblem(reg, "p2", static=2)
#' addAlgorithm(reg, id="a1",
#'   fun=function(static, dynamic, alpha) c(y=static*alpha))
#' addAlgorithm(reg, id="a2",
#'   fun=function(static, dynamic, alpha, beta) c(y=static*alpha+beta))
#' ad1 <- makeDesign("a1", exhaustive=list(alpha=1:2))
#' ad2 <- makeDesign("a2", exhaustive=list(alpha=1:2, beta=5:6))
#' addExperiments(reg, algo.designs=list(ad1, ad2), repls=2)
#' submitJobs(reg)
#' waitForJobs(reg)
#' data <- reduceResultsExperiments(reg)
#' library(plyr)
#' ddply(data, getResultVars(data, "group"), summarise, mean_y = mean(y))
#' }
getResultVars = function(data, type="group") {
  checkArg(data, "ReducedResultsExperiments")
  checkArg(type, choices=c("prob", "prob.pars", "algo", "algo.pars", "group", "result"))
  switch(type,
    prob = c("prob", attr(data, "prob.pars.names")),
    prob.pars = attr(data, "prob.pars.names"),
    algo = c("algo", attr(data, "algo.pars.names")),
    algo.pars = attr(data, "algo.pars.names"),
    group = c("prob", "algo", attr(data, "prob.pars.names"), attr(data, "algo.pars.names")),
    result = setdiff(colnames(data), c("id", "algo", "prob", "repl", attr(data, "prob.pars.names"), attr(data, "algo.pars.names")))
  )
}
