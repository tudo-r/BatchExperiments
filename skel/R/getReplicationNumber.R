#' Get replication number of jobs.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of jobs.
#'   Default is all jobs.
#' @return [\code{integer}]. Replication numbers for jobs, named with ids.
#' @export
getReplicationNumber = function(reg, ids) {
  checkArg(reg, cl = "ExperimentRegistry")
  if (!missing(ids))
    ids = BatchJobs:::checkIds(reg, ids)

  tab = dbGetReplicationNumber(reg, ids)
  setNames(tab$repl, tab$job_id)
}
