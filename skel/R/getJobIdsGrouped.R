# Get job ids grouped by algorithms.
# @param reg [\code{\link{ExperimentRegistry}}]\cr
#   Registry.
# @return [\code{list}]. Named list of integer job ids.
# @export
# FIXME not done, scoping hell
getJobIdsGrouped = function(reg, ids, by.prob=FALSE, by.algo=FALSE, by.repl=FALSE, by.prob.pars, by.algo.pars, envir = parent.frame()) {
  force(envir)
  f = getIndex(reg, ids = ids, by.prob, by.algo, by.repl, by.prob.pars, by.algo.pars)
  if (missing(ids)) # yep, we do this after getting the index to avoid some checks and queries
    ids = BatchJobs:::dbGetJobIds(reg)
  if (!length(f))
    return(ids)
  split(ids, f)
}
