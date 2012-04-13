#' Remove algorithm from registry.
#'
#' THIS DELETES ALL FILES REGARDING THIS ALGORITHM, INCLUDING ALL JOBS AND RESULTS!
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param id [\code{character(1)}]\cr
#'   Id of algorithm.
#' @return Nothing.
#' @export
removeAlgorithm = function(reg, id) {
  checkArg(reg, "ExperimentRegistry")
  checkArg(id, "character", len=1L, na.ok=FALSE)

  if (!(id %in% dbGetAlgorithmIds(reg)))
    stop("Algorithm not present in registry: ", id)

  message("Removing Experiments from database")
  ids = dbFindExperiments(reg, algo.pattern=id, like=FALSE)
  removeExperiments(reg, ids=ids)
  message("Removing Algorithm from database")
  dbRemoveAlgorithm(reg, id)
  
  fn = getAlgorithmFilePath(reg$file.dir, id)
  message("Deleting algorithm file: ", fn)
  ok = file.remove(fn)
  if (!ok)
    warningf("Could not remove algorithm file: %s", fn)
  invisible(NULL)
}
