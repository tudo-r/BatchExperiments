#' Print textual information about selected experiments.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Selected experiments.
#'   Default is all experiments.
#' @param details [\code{logical(1)}]\cr
#'   Should detailed information for each single experiment be printed?
#'   Default is \code{FALSE}.
#' @return \code{\link{table}} of problems and algorithms.
#' @export
summarizeExperiments = function(reg, ids, details=FALSE) {
  checkArg(reg, "ExperimentRegistry")
  if (! missing(ids))
    ids = BatchJobs:::checkIds(reg, ids)

  tab = BatchJobs:::dbGetExpandedJobsTable(reg, ids, c("job_id", "prob_id", "algo_id"))
  tab = table(Problem = tab$prob_id, Algorithm = tab$algo_id)
  if (!details)
    return(tab)

  print(tab)
  jobs = getJobs(reg, ids, check.ids=FALSE)
  message("\nExperiment details:\n")
  fmt = paste("Job: %s",
              "  Problem: %s", "  Problem parameters: %s",
              "  Algorithm: %s", "  Algorithm parameters: %s",
              "  Replication: %i\n", sep = "\n")

  lapply(jobs, function(j) {
         messagef(fmt, j$id,
                  j$prob.id, listToShortString(j$prob.pars),
                  j$algo.id, listToShortString(j$algo.pars),
                  j$repl)

              })
  return(invisible(tab))
}
