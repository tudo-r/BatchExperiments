#' Remove jobs from registry.
#'
#' THIS DELETES ALL FILES REGARDING THE JOBS, INCLUDING RESULTS!
#' If you really know what you are doing, you may set \code{force}
#' to \code{TRUE} to omit sanity checks on running jobs.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs you want to remove.
#'   Default is none.
#' @param force [\code{logical(1)}]\cr
#'   Also remove jobs which seem to be still running.
#'   Default is \code{FALSE}.
#' @return Vector of type \code{integer} of removed job ids.
#' @export
removeExperiments = function(reg, ids, force=FALSE) {
  checkArg(reg, "Registry")
  if (missing(ids))
    return(integer(0L))
  ids = BatchJobs:::checkIds(reg, ids)

  if (!force) {
    if(is.null(BatchJobs:::getListJobs()) || is.null(BatchJobs:::getKillJob())) {
      stop("Listing or killing of jobs not supported by your cluster functions\n",
           "You need to set force=TRUE to remove jobs, but note the warning in ?removeExperiments")
    }
    running = findRunning(reg, ids)
    if (length(running) > 0L)
      stopf("Can't remove jobs which are still running. You have to kill them first.\nRunning: %s",
            collapse(running))
  }

  messagef("Removing %i experiments ...", length(ids))
  BatchJobs:::dbRemoveJobs(reg, ids)

  fmt = "^%i(\\.(R|out)|-result(-.+)*\\.RData)$"
  lapply(ids, function(id) {
    fs = list.files(BatchJobs:::getJobDirs(reg, id), pattern=sprintf(fmt, id), full.names=TRUE)
    ok = file.remove(fs)
    if (!all(ok))
      warningf("Could not remove files for experiment with id=%i", id)
  })

  invisible(ids)
}
