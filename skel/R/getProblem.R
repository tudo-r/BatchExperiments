#' Get problem from registry by id.
#' The object is loaded from disk.
#' @param reg [\code{\link{Registry}}]\cr
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
  prob = loadProblem(reg$file.dir, id)
  #FIXME this should be an option of loadProblem
  query = sprintf("SELECT pseed FROM %s_prob_def WHERE prob_id = '%s'", reg$id, id)
  prob$seed = BatchJobs:::dbDoQuery(reg, query)$pseed
  prob
}
