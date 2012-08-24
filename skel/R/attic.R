makeReplicatedExperiment = function(id=NA_integer_, prob.id, prob.pars, algo.id, algo.pars, repls) {
  structure(list(id=id, prob.id=prob.id, prob.pars=prob.pars, algo.id=algo.id,
                 algo.pars=algo.pars, repls=repls),
            class="ReplicatedExperiment")
}

dbGetReplicatedExperiments = function(reg, ids) {
  query = sprintf("SELECT job_id, job_def_id, prob_id, prob_pars, algo_id, algo_pars, COUNT(job_id) AS repls FROM %s_expanded_jobs",
                  reg$id)
  tab = BatchJobs:::dbSelectWithIds(reg, query, ids, group.by="job_def_id")

  lapply(seq_len(nrow(tab)), function(i) {
    x = tab[i,]
    prob.pars = unserialize(charToRaw(x$prob_pars))
    algo.pars = unserialize(charToRaw(x$algo_pars))
    makeReplicatedExperiment(id=x$job_def_id, prob.id=x$prob_id, prob.pars=prob.pars,
                                algo.id=x$algo_id, algo.pars=algo.pars, repls=x$repls)
  })
}

summarizeExperiments.old = function(reg, ids, show=c("prob", "algo")) {
  checkArg(reg, "ExperimentRegistry")
  checkArg(show, "character", min.len=1, na.ok=FALSE)
  # FIXME: the following code is still a bit slow
  # but we gain much by handling repls in the DB
  rexps = dbGetReplicatedExperiments(reg, ids)
  prob = do.call(rbind.fill, lapply(rexps, function(e)
    if (length(e$prob.pars) == 0L)
      data.frame(prob=e$prob.id)
    else
      cbind(prob=e$prob.id, as.data.frame(e$prob.pars))
  ))
  algo = do.call(rbind.fill, lapply(rexps, function(e)
    if (length(e$algo.pars) == 0L)
      data.frame(algo=e$algo.id)
    else
      cbind(algo=e$algo.id, as.data.frame(e$algo.pars))
  ))
  repls = extractSubList(rexps, "repls", integer(1L))
  d = as.data.frame(cbind(prob, algo, repls=repls))
  diff = setdiff(show, colnames(d))
  if (length(diff) > 0)
    stopf("Trying to select columns in arg 'show' which are not available: %s", collapse(diff))
  ddply(d, show, function(x) c(.count=sum(x$repls)))
}
