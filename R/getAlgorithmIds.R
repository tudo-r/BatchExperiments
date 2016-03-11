#' @title Get ids of algorithms in registry.
#'
#' @description
#' Get algorithm ids for jobs.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param ids [code{integer}]\cr
#'   Job ids to restrict returned algorithm ids to.
#' @return [\code{character}].
#' @family get
#' @export
getAlgorithmIds = function(reg, ids) {
  checkExperimentRegistry(reg, strict = TRUE, writeable = FALSE)
  if (missing(ids))
    return(dbGetAllAlgorithmIds(reg))
  ids = checkIds(reg, ids)
  unique(dbGetAlgorithmIds(reg, ids))
}
