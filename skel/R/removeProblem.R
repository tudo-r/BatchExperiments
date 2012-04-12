#' Remove problem from registry.
#'
#' THIS DELETES ALL FILES REGARDING THIS PROBLEM, INCLUDING ALL JOBS AND RESULTS!
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param id [\code{character(1)}]\cr
#'   Id of problem.
#' @return Nothing.
#' @export
removeProblem = function(reg, id) {
  checkArg(reg, "ExperimentRegistry")
  checkArg(id, "character", len=1L, na.ok=FALSE)

  if (!(id %in% dbGetProblemIds(reg)))
    stop("Problem not present in registry: ", id)


  fn = getProblemFilePath(reg$file.dir, id)
  message("Deleting problem file: ", fn)
  ok = file.remove(fn)
  if (!ok)
    stop("Could not remove problem file: ", fn)
  ids = dbFindExperiments(reg, prob.pattern=id, like=FALSE)
  removeExperiments(reg, ids=ids)
  #FIXME it might be better to first clean the db and then the files
  #FIMXE orphaned files do no harm, but jobs with no result files do!
  dbRemoveProblem(reg, id)
  invisible(NULL)
}
