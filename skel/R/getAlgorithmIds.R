#' Get ids of algorithms in registry.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @return [\code{character}].
#' @export
getAlgorithmIds = function(reg) {
  BatchJobs:::checkRegistry(reg, "ExperimentRegistry", strict=TRUE)
  dbGetAlgorithmIds(reg)
}
