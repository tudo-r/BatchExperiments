#' @method getJobInfo ExperimentRegistry
#' @S3method getJobInfo ExperimentRegistry
getJobInfo.ExperimentRegistry = function(reg, ids, parameters=FALSE, select, unit = "seconds") {
  BatchJobs:::syncRegistry(reg)
  if (!missing(ids))
    ids = BatchJobs:::checkIds(reg, ids)
  checkArg(unit, choices=c("seconds", "minutes", "hours", "days", "weeks"))
  checkArg(parameters, "logical", len=1L, na.ok=FALSE)

  columns = setNames(c("job_id", "prob_id", "algo_id", "repl", "submitted", "started", "done", "done - started AS time_running", "submitted - started AS time_queued", "error", "node", "batch_job_id", "r_pid", "seed"),
                     c("job.id", "prob", "algo", "repl", "time.submitted", "time.started", "time.done", "time.running", "time.queued", "error.msg", "nodename", "batch.id", "r.pid", "seed"))
  if (parameters)
    columns = c(columns, setNames(c("prob_pars", "algo_pars"), c("prob.pars", "algo.pars")))

  if (!missing(select)) {
    checkArg(select, "character", na.ok=FALSE)
    columns = columns[names(columns) %in% c("job.id", select)]
  }

  tab = setNames(BatchJobs:::dbGetExpandedJobsTable(reg, ids, columns), names(columns))
  if (nrow(tab) == 0L)
    return(tab)

  # convert times to POSIX
  if (!is.null(tab$time.submitted))
    tab$time.submitted = BatchJobs:::dbConvertNumericToPOSIXct(tab$time.submitted)
  if (!is.null(tab$time.started))
    tab$time.started = BatchJobs:::dbConvertNumericToPOSIXct(tab$time.started)
  if (!is.null(tab$time.done))
    tab$time.done = BatchJobs:::dbConvertNumericToPOSIXct(tab$time.done)

  # shorten error messages
  if (!is.null(tab$error.msg))
    tab$error.msg = vapply(tab$error.msg, BatchJobs:::shortenString, "", len = 30L)

  # convert time diffs
  div = setNames(c(1L, 60L, 3600L, 86400L, 604800L), c("seconds", "minutes", "hours", "days", "weeks"))[unit]
  if (!is.null(tab$time.running))
    tab$time.running = prettyNum(tab$time.running / div)
  if (!is.null(tab$time.queued))
    tab$time.queued = prettyNum(tab$time.queued / div)

  # unserialize parameters
  if (parameters) {
    if (!is.null(tab$prob.pars)) {
      pars = BatchJobs:::list2df(lapply(tab$prob.pars, function(x) unserialize(charToRaw(x))), force.names=TRUE)
      names(pars) = sprintf("prob.par.%s", names(pars))
      tab = cbind(subset(tab, select=-prob.pars), pars)
    }
    if (!is.null(tab$algo.pars)) {
      pars = BatchJobs:::list2df(lapply(tab$algo.pars, function(x) unserialize(charToRaw(x))), force.names=TRUE)
      names(pars) = sprintf("algo.par.%s", names(pars))
      tab = cbind(subset(tab, select=-algo.pars), pars)
    }
  }

  tab
}
