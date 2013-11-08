#' @method getJobInfo ExperimentRegistry
#' @S3method getJobInfo ExperimentRegistry
getJobInfo.ExperimentRegistry = function(reg, ids, pars=FALSE, prefix.pars=FALSE, select, unit = "seconds") {
  BatchJobs:::syncRegistry(reg)
  checkArg(pars, "logical", len=1L, na.ok=FALSE)
  columns = c(id="job_id", prob="prob_id", algo="algo_id", repl="repl")
  if (pars)
    columns = c(columns, c(prob.pars="prob_pars", algo.pars="algo_pars"))
  tab = BatchJobs:::getJobInfoInternal(reg, ids, pars, select, unit, columns)

  # unserialize parameters
  if (pars) {
    if (!is.null(tab$prob.pars)) {
      pars = BatchJobs:::list2df(lapply(tab$prob.pars, function(x) unserialize(charToRaw(x))), force.names=TRUE)
      if (prefix.pars)
        names(pars) = sprintf("prob.par.%s", names(pars))
      tab = cbind(subset(tab, select=setdiff(names(tab), "prob.pars")), pars)
    }
    if (!is.null(tab$algo.pars)) {
      pars = BatchJobs:::list2df(lapply(tab$algo.pars, function(x) unserialize(charToRaw(x))), force.names=TRUE)
      if (prefix.pars)
        names(pars) = sprintf("algo.par.%s", names(pars))
      tab = cbind(subset(tab, select=setdiff(names(tab), "algo.pars")), pars)
    }
  }
  return(tab)
}
