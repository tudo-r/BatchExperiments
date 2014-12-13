#' @title Get all parts required to run a single job.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param ids [\code{integer(1)}]\cr
#'   Id of a job.
#' @return [named list]. Returns the \link{Job}, \link{Problem} and \link{Instance}.
#' @family get
#' @export
getExperimentParts = function(reg, id) {
  checkExperimentRegistry(reg, strict = TRUE)
  id = BatchJobs:::checkId(reg, id)

  res = namedList(c("job", "prob", "instance"))
  res$job = dbGetJobs(reg, id)[[1L]]
  res$prob = loadProblem(reg, id)
  # use insert to keep the slot even if this is NULL
  insert(res, list(instance = calcDynamic(reg, job, prob$static, prob$dynamic)))
}
