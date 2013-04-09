#' Get algorithm from registry by id.
#' The object is loaded from disk.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param id [\code{character(1)}]\cr
#'   Id of algorithm.
#' @return [\code{\link{Algorithm}}].
#' @export
getAlgorithm = function(reg, id) {
  checkExperimentRegistry(reg, strict=TRUE)
  checkArg(id, "character", na.ok=FALSE, len=1L)
  aids = dbGetAlgorithmIds(reg)
  if (id %nin% aids)
    stop("Unknown algorithm id, possible candidates are: ", collapse(aids))
  loadAlgorithm(reg$file.dir, id)
}
