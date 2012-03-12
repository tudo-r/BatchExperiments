#' @S3method dbCreateJobDefTable ExperimentRegistry
dbCreateJobDefTable.ExperimentRegistry = function(reg) {
  message("Initializing experiment definition table...")

  query = sprintf(paste("CREATE TABLE %s_job_def (job_def_id INTEGER PRIMARY KEY,", 
                        "prob_id TEXT, prob_pars TEXT, algo_id TEXT, algo_pars TEXT,",
                        "UNIQUE(prob_id, prob_pars, algo_id, algo_pars))"), reg$id)
  BatchJobs:::dbDoQuery(reg, query, flags="rwc")
  BatchJobs:::dbCreateExpandedJobsView(reg)
}


dbCreateExtraTables = function(reg) {
  message("Initializing prob and algo tables...")
  query = sprintf("CREATE TABLE %s_prob_def (id TEXT PRIMARY KEY)", reg$id) 
  BatchJobs:::dbDoQuery(reg, query, flags="rwc")
  query = sprintf("CREATE TABLE %s_algo_def (id TEXT PRIMARY KEY)", reg$id) 
  BatchJobs:::dbDoQuery(reg, query, flags="rwc")
}


#' @method dbGetJobs ExperimentRegistry
#' @S3method dbGetJobs ExperimentRegistry
dbGetJobs.ExperimentRegistry = function(reg, ids) {
  query = sprintf("SELECT job_id,prob_id,prob_pars,algo_id,algo_pars,seed,repl FROM %s_expanded_jobs", reg$id)
  if (missing(ids)) {
    tab = BatchJobs:::dbDoQuery(reg, query)
  } else {
    query = sprintf("%s WHERE job_id IN ('%s')", query, collapse(ids, sep="','"))
    tab = BatchJobs:::dbDoQuery(reg, query)
    if(nrow(tab) == 0L) 
      stopf("No jobs found for ids: %s", collapse(ids))
    tab = tab[match(ids, tab$job_id),, drop=FALSE]
  }
  
  lapply(seq_len(nrow(tab)), function(i) {
    x = tab[i,]  
    prob.pars = unserialize(charToRaw(x$prob_pars))
    algo.pars = unserialize(charToRaw(x$algo_pars))
    makeExperimentJob(id=x$job_id, prob.id=x$prob_id, prob.pars=prob.pars, 
      algo.id=x$algo_id, algo.pars=algo.pars, seed=x$seed, repl=x$repl)
  })
}

dbFindExperiments = function(reg, prob.pattern, algo.pattern, repls, like=TRUE) {
  clause = character(0L)
  if (!missing(repls))
    clause = c(clause, sprintf("repl IN ('%s')", collapse(repls)))

  if (!missing(prob.pattern)) {
    if (like)
      clause = c(clause, sprintf("prob_id LIKE '%%%s%%'", prob.pattern))
    else  
      clause = c(clause, sprintf("prob_id = '%s'", prob.pattern))
  } 
  if (!missing(algo.pattern)) {
    if (like)
      clause = c(clause, sprintf("algo_id LIKE '%%%s%%'", algo.pattern))
    else  
      clause = c(clause, sprintf("algo_id = '%s'", algo.pattern))
  }

  query = sprintf("SELECT job_id from %s_expanded_jobs", reg$id)
  if (length(clause) > 0L)
    query = paste(query, "WHERE", collapse(clause, sep = " AND "))
  as.integer(BatchJobs:::dbDoQuery(reg, query)$job_id)
}

dbAddProblem = function(reg, id) {
  query = sprintf("INSERT INTO %s_prob_def (id) VALUES ('%s')", reg$id, id)
  BatchJobs:::dbDoQuery(reg, query, flags="rw")
}

dbAddAlgorithm = function(reg, id) {
  query = sprintf("INSERT INTO %s_algo_def (id) VALUES ('%s')", reg$id, id)
  BatchJobs:::dbDoQuery(reg, query, flags="rw")
}

dbRemoveProblem = function(reg, id) {
  query = sprintf("DELETE FROM %s_prob_def WHERE id='%s'", reg$id, id)
  BatchJobs:::dbDoQuery(reg, query, flags="rw")
}

dbRemoveAlgorithm = function(reg, id) {
  query = sprintf("DELETE FROM %s_algo_def WHERE id='%s'", reg$id, id)
  BatchJobs:::dbDoQuery(reg, query, flags="rw")
}

dbGetProblemIds = function(reg) {
  query = sprintf("SELECT id FROM %s_prob_def", reg$id)
  BatchJobs:::dbDoQuery(reg, query)$id
}

dbGetAlgorithmIds = function(reg) {
  query = sprintf("SELECT id FROM %s_algo_def", reg$id)
  BatchJobs:::dbDoQuery(reg, query)$id
}


