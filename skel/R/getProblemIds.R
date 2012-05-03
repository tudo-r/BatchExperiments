#' Get ids of problems in registry.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @return [\code{character}].
#' @export
getProblemIds = function(reg) {
  checkArg(reg, "ExperimentRegistry")
  dbGetProblemIds(reg)
}
