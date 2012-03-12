#' Get ids of algorithms in registry.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{character}].
#' @export
getAlgorithmIds = function(reg) {
  dbGetAlgorithmIds(reg)
}
