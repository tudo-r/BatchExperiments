#' Get ids of problems in registry.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @return [\code{character}].
#' @export
getProblemIds = function(reg) {
  BatchJobs:::checkRegistry(reg, "ExperimentRegistry", strict=TRUE)
  dbGetProblemIds(reg)
}
