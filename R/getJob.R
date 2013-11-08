#' Get jobs (here: experiments) from registry by id.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of job.
#'   Default is all jobs.
#' @param load.fun [\code{logical(1)}]\cr
#'   Load job function from disk? Not useful to set to \code{TRUE} in BatchExperiments.
#'   Default is \code{FALSE}.
#' @param check.ids [\code{logical(1)}]\cr
#'   Check the job ids?
#'   Default is \code{TRUE}.
#' @return [list of \code{Experiment}].
#' @method getJobs ExperimentRegistry
#' @S3method getJobs ExperimentRegistry
#' @export
getJobs.ExperimentRegistry = function(reg, ids, load.fun=FALSE, check.ids=TRUE) {
  if (!missing(ids) && check.ids)
    ids = BatchJobs:::checkIds(reg, ids)
  dbGetJobs(reg, ids)
}
