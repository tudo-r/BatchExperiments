#' Generate dynamic part of problem.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param id [\code{character(1)}]\cr
#'   Id of job.
#' @return Dynamic part of problem.
#' @export
generateProblemInstance = function(reg, id) {
  job = getJob(reg, id)
  prob = getProblem(reg, job$prob.id)
  calcDynamic(reg, job, prob$static, prob$dynamic)
}
