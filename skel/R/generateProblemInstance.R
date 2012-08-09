#' Generate dynamic part of problem.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param id [\code{character(1)}]\cr
#'   Id of job.
#' @return Dynamic part of problem.
#' @export

generateProblemInstance = function(reg, id) {
  checkArg(reg, "ExperimentRegistry")
  checkArg(id, cl = "integer", len=1L, na.ok = FALSE)

  dynamic = getDynamic(reg, getJob(reg, id, check.id=FALSE))
  dynamic()
}
