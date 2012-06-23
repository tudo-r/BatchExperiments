#' @S3method dbCreateJobDefTable ExperimentRegistry
dbCreateJobDefTable.ExperimentRegistry = function(reg) {
  #message("Initializing experiment definition table...")
  query = sprintf(paste("CREATE TABLE %s_job_def (job_def_id INTEGER PRIMARY KEY,",
                        "prob_id TEXT, prob_pars TEXT, algo_id TEXT, algo_pars TEXT,",
                        "UNIQUE(prob_id, prob_pars, algo_id, algo_pars))"), reg$id)
  BatchJobs:::dbDoQuery(reg, query, flags="rwc")
}

dbCreateExtraTables = function(reg) {
  #message("Initializing prob and algo tables...")
  query = sprintf("CREATE TABLE %s_prob_def (prob_id TEXT PRIMARY KEY, pseed INTEGER)", reg$id)
  BatchJobs:::dbDoQuery(reg, query, flags="rwc")
  query = sprintf("CREATE TABLE %s_algo_def (algo_id TEXT PRIMARY KEY)", reg$id)
  BatchJobs:::dbDoQuery(reg, query, flags="rwc")
}

dbCreateExpandedJobsViewBE = function(reg) {
  query = sprintf(paste("CREATE VIEW %1$s_expanded_jobs AS",
                        "SELECT * FROM %1$s_job_status AS job_status",
                        "LEFT JOIN %1$s_job_def AS job_def USING(job_def_id)",
                        "LEFT JOIN %1$s_prob_def AS prob_def USING (prob_id)"), reg$id)
  BatchJobs:::dbDoQuery(reg, query, flags="rw")
}

#' @method dbGetJobs ExperimentRegistry
#' @S3method dbGetJobs ExperimentRegistry
dbGetJobs.ExperimentRegistry = function(reg, ids) {
  cols = c("job_id", "prob_id", "prob_pars", "algo_id",
           "algo_pars", "seed", "prob_seed", "repl")
  query = sprintf("SELECT %s FROM %s_expanded_jobs", collapse(cols), reg$id)
  tab = BatchJobs:::dbSelectWithIds(reg, query, ids)

  lapply(seq_len(nrow(tab)), function(i) {
    x = tab[i,]
    prob.pars = unserialize(charToRaw(x$prob_pars))
    algo.pars = unserialize(charToRaw(x$algo_pars))
    makeExperimentJob(id=x$job_id, prob.id=x$prob_id, prob.pars=prob.pars,
      algo.id=x$algo_id, algo.pars=algo.pars, seed=x$seed, repl=x$repl, prob.seed = x$prob_seed)
  })
}

dbFindExperiments = function(reg, prob.pattern, algo.pattern, repls, like=TRUE) {
  clause = character(0L)
  if (!missing(repls))
    clause = c(clause, sprintf("repl IN (%s)", collapse(repls)))
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
  BatchJobs:::dbDoQuery(reg, query)$job_id
}

dbAddProblem = function(reg, id, seed) {
  #TODO: replace OR REPLACE with an option, this is not supported by all DBMS
  query = sprintf("INSERT OR REPLACE INTO %s_prob_def (prob_id, pseed) VALUES ('%s', %s)",
                  reg$id, id, ifelse(is.null(seed), "NULL", seed))
  BatchJobs:::dbDoQuery(reg, query, flags="rw")
}

dbAddAlgorithm = function(reg, id) {
  #TODO: replace OR REPLACE with an option, this is not supported by all DBMS
  query = sprintf("INSERT OR REPLACE INTO %s_algo_def (algo_id) VALUES ('%s')", reg$id, id)
  BatchJobs:::dbDoQuery(reg, query, flags="rw")
}

dbRemoveProblem = function(reg, id) {
  query = sprintf("DELETE FROM %s_prob_def WHERE prob_id='%s'", reg$id, id)
  BatchJobs:::dbDoQuery(reg, query, flags="rw")
}

dbRemoveAlgorithm = function(reg, id) {
  query = sprintf("DELETE FROM %s_algo_def WHERE algo_id='%s'", reg$id, id)
  BatchJobs:::dbDoQuery(reg, query, flags="rw")
}

dbGetProblemIds = function(reg) {
  query = sprintf("SELECT prob_id FROM %s_prob_def", reg$id)
  BatchJobs:::dbDoQuery(reg, query)$prob_id
}

dbGetAlgorithmIds = function(reg) {
  query = sprintf("SELECT algo_id FROM %s_algo_def", reg$id)
  BatchJobs:::dbDoQuery(reg, query)$algo_id
}

dbGetProblemIdsNotAdded = function(reg) {
  query = sprintf("SELECT prob_id FROM %1$s_prob_def EXCEPT SELECT DISTINCT prob_id FROM %1$s_job_def", reg$id)
  BatchJobs:::dbDoQuery(reg, query)$prob_id
}

dbGetAlgorithmIdsNotAdded = function(reg) {
  query = sprintf("SELECT algo_id FROM %1$s_algo_def EXCEPT SELECT DISTINCT algo_id FROM %1$s_job_def", reg$id)
  BatchJobs:::dbDoQuery(reg, query)$prob_id
}
