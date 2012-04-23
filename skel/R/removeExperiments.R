#' Remove jobs from registry.
#'
#' THIS DELETES ALL FILES REGARDING THE JOBS, INCLUDING RESULTS!
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs you want to remove.
#'   Default is none.
#' @return Nothing.
#' @export
removeExperiments = function(reg, ids) {
  checkArg(reg, "Registry")
  if (missing(ids))
    return(invisible(NULL))
  ids = BatchJobs:::checkIds(reg, ids)

  messagef("Removing %i experiments ...", length(ids))
  BatchJobs:::dbRemoveJobs(reg, ids)

  fmt = "^%i(\\.(R|out)|-result(-.+)*\\.RData)$"
  lapply(ids, function(id) {
    fs = list.files(BatchJobs:::getJobDirs(reg, id), pattern=sprintf(fmt, id), full.names=TRUE)
    ok = file.remove(fs)
    if (!all(ok))
      warningf("Could not remove files for experiment with id=%i", id)
  })

  invisible(NULL)
}
