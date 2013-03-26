#' Generate dynamic part of problem.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param id [\code{character(1)}]\cr
#'   Id of job.
#' @return Dynamic part of problem.
#' @export
generateProblemInstance = function(reg, id) {
  BatchJobs:::checkRegistry(reg, "ExperimentRegistry", strict=TRUE)
  id = BatchJobs:::checkId(reg, id)

  dynamic = getDynamicLazy(reg, getJob(reg, id, check.id=FALSE))
  dynamic()
}
