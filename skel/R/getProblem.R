#' Get problem from registry by id.
#'
#' The object is loaded from disk.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param id [\code{character(1)}]\cr
#'   Id of problem.
#' @return [\code{\link{Problem}}].
#' @export
getProblem = function(reg, id) {
  checkArg(reg, "ExperimentRegistry")
  checkArg(id, "character", na.ok=FALSE, len=1L)
  pids = dbGetProblemIds(reg)
  if (!(id %in% pids))
    stop("Unknown problem id, possible candidates are: ", collapse(pids))
  loadProblem(reg, id, seed=TRUE)
}
