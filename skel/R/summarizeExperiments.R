#' Print textual information about selected experiments.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Selected experiments.
#'   Default is all experiments.
#' @param details [\code{logical(1)}]\cr
#'   Should individual information for each single experiment be printed or only a short table?
#'   Default is \code{FALSE}.
#' @return Nothing.
#' @export
summarizeExperiments = function(reg, ids, details=FALSE) {
  checkArg(reg, "ExperimentRegistry")
  if (! missing(ids)) {
    ids = convertIntegers(ids)
    BatchJobs:::checkIds(reg, ids)
  }

  tab = BatchJobs:::dbGetExpandedJobsTable(reg, ids, c("job_id", "prob_id", "algo_id"))
  if(nrow(tab) == 0L)
    stopf("No experiments in registry or none matching id found")
  print(table(Problem = tab$prob_id, Algorithm = tab$algo_id))

  if (details) {
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
  }
  invisible(NULL)
}
