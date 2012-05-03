#' Get ids of algorithms in registry.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @return [\code{character}].
#' @export
getAlgorithmIds = function(reg) {
  checkArg(reg, "ExperimentRegistry")
  dbGetAlgorithmIds(reg)
}
