#' Get ids of problems in registry.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return [\code{character}].
#' @export
getProblemIds = function(reg) {
  dbGetProblemIds(reg)
}
