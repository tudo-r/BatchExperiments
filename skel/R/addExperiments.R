#' Add experiments for running algorithms on problems
#' to the registry, so they can be executed later.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param prob.designs [\code{character} | \code{\link{Design}} | list of \code{\link{Design}}]\cr
#'   Either problem ids, a single problem design or a list of problem designs,
#'   the latter two created by \code{\link{makeDesign}}.
#'   If missing, all problems are selected (without associating a design),
#'   and this is the default.
#' @param algo.designs [\code{character} | \code{\link{Design}} | list of \code{\link{Design}}]\cr
#'   Either algorithm ids, a single algorithm design or a list of problem algorithm,
#'   the latter two created by \code{\link{makeDesign}}.
#'   If missing, all algorithms are selected (without associating a design),
#'   and this is the default.
#' @param repls [\code{integer(1)}]\cr
#'   Number of replications.\cr
#'   Default is 1.
#' @param skip.defined [\code{logical}]\cr
#'   If set to \code{TRUE}, already defined experiments get skipped. Otherwise an error is thrown.\cr
#'   Default is FALSE.
#' @return Invisibly returns vector of ids of added experiments.
#' @export
#' @aliases Experiment
addExperiments = function(reg, prob.designs, algo.designs, repls=1L, skip.defined=FALSE) {
  checkArg(reg, cl = "ExperimentRegistry")
  repls = convertIntegers(repls)
  checkArg(repls, "integer", len=1, lower=1L, na.ok=FALSE)
  skip.defined = as.logical(skip.defined)
  checkArg(skip.defined, "logical", na.ok=FALSE)
  
  if (missing(prob.designs))
    prob.designs = getProblemIds(reg)

  # check prob.designs
  if (is.character(prob.designs)) {
    prob.designs = lapply(prob.designs, function(id) makeDesign(id))
  } else if (is(prob.designs, "Design")) {
    prob.designs = list(prob.designs)
  } else if (is.list(prob.designs)) {
    if (length(prob.designs) == 0L)
      stop("'prob.designs' is empty!")
    checkListElementClass(prob.designs, "Design")
  } else {
    stop("Format of prob.designs not supported. Must be a character, a design or list of designs")
  }
  ids = unique(extractSubList(prob.designs, "id"))
  found = ids %in% getProblemIds(reg)
  if (! all(found))
    stopf("%i problems have not been added to registry for designs: %s",
          sum(!found), collapse(ids[!found]))


  # check algo.designs
  if (missing(algo.designs))
    algo.designs = getAlgorithmIds(reg)
  if (is.character(algo.designs)) {
    algo.designs = lapply(algo.designs, function(id) makeDesign(id))
  } else if (is(algo.designs, "Design")) {
    algo.designs = list(algo.designs)
  } else if (is.list(algo.designs)) {
    if (length(algo.designs) == 0L)
      stop("'algo.designs' is empty!")
    checkListElementClass(algo.designs, "Design")
  } else {
    stop("Format of algo.designs not supported. Must be a character, a design or list of designs")
  }
  ids = unique(extractSubList(algo.designs, "id"))
  found = ids %in% getAlgorithmIds(reg)
  if (! all(found))
    stopf("%i algorithms have not been added to registry for designs: %s",
          sum(!found), collapse(ids[!found]))


  # internal helper functions
  mq = function(lines, ..., con=NULL, bind.data=NULL) {
    q = sprintf(collapse(lines, sep=" "), ...)
    if(is.null(con))
      return(q)
    if(is.null(bind.data))
      return(dbGetQuery(con, q))
    return(dbGetPreparedQuery(con, q, bind.data = bind.data))
  }
  
  seripars = function(x) {
    rawToChar(serialize(x, connection=NULL, ascii=TRUE))
  }
 
  writeJobDefs = function(job.defs, n) {
    messagef("Creating %i job definitions", n)
    data = as.data.frame(do.call(rbind, lapply(head(job.defs, n), unlist)))
    mq("INSERT INTO tmp(prob_id, prob_pars, algo_id, algo_pars) VALUES(?, ?, ?, ?)",
       con = con, bind.data = data)
  }


  # establish persistent connection and create temporary table to fill
  # with job definitions
  con = BatchJobs:::dbConnectToJobsDB(reg, "rw")
  on.exit(dbDisconnect(con))

  # create temporary table for job definitions
  mq(c("CREATE TEMP TABLE tmp(tid INTEGER PRIMARY KEY,",
       "job_def_id INTEGER, prob_id TEXT, prob_pars TEXT,",
       "algo_id TEXT, algo_pars TEXT)"), con = con)

  # write auxiliary temporary table with replication numbers
  mq("CREATE TEMP TABLE repls(repl INTEGER)", con = con)
  mq("INSERT INTO repls(repl) VALUES(?)", 
     con = con, bind.data = data.frame(repl=seq_len(repls)))

  # create temporary view on cross product of repls and job_def_id 
  mq(c("CREATE TEMP VIEW cp AS SELECT repls.repl, tmp.job_def_id FROM tmp",
     "CROSS JOIN repls"), con = con)

  
  # iterate to generate job definitions
  # write to temporary table every x definitions
  at.once = 5000L
  job.defs = vector("list", at.once)
  n = 0L
  for (pd in prob.designs) {
    pd$designIter$reset()
    while (pd$designIter$hasNext()) {
      prob.pars = seripars(pd$designIter$nextElem())
      for (ad in algo.designs) {
        ad$designIter$reset()
        while (ad$designIter$hasNext()) {
          algo.pars = seripars(ad$designIter$nextElem())

          n = n + 1L
          job.defs[[n]] = list(prob_id = pd$id, prob_pars = prob.pars, 
                               algo_id = ad$id, algo_pars = algo.pars)
          if(n == at.once) {
            writeJobDefs(job.defs, n)
            n = 0L
          }
        }
      }
    }
  }

  # add (remaining) defs to temporary job_defs table
  if(n > 0L)
    writeJobDefs(job.defs, n)
  rm(job.defs)

  # query job_id to keep track of new ids
  max.job.id = mq("SELECT COALESCE(MAX(job_id), 0) AS x FROM %s_job_status", reg$id, con = con)$x
  
  # match for known job_def_id
  mq(c("UPDATE tmp SET job_def_id = (SELECT job_def_id FROM %s_job_def AS jd",
       "WHERE jd.prob_id = tmp.prob_id AND jd.algo_id = tmp.algo_id AND", 
       "jd.prob_pars = tmp.prob_pars AND jd.algo_pars = tmp.algo_pars)"), 
     reg$id, con = con)

  if(!skip.defined) {
    n.dup = mq("SELECT COUNT(job_def_id) AS n FROM tmp", con = con)$n
    if(n.dup > 0L) {
      stop(paste("You have added identical experiments.",
        "Mabye you have duplicated problem or algorithm ids?",
        "If this is intended and you know what you're doing, try skip.defined=TRUE.",
        sep = "\n"))
    }
  }

  dbBeginTransaction(con)
  ok = try({
    message("Writing job definitions ...")
    # insert new job defs
    mq(c("INSERT INTO %s_job_def(prob_id, prob_pars, algo_id, algo_pars)",
         "SELECT prob_id, prob_pars, algo_id, algo_pars FROM tmp",
         "WHERE job_def_id IS NULL"), reg$id, con = con)

    message("Writing job definitions ...")
    # update temporary table with new job defs
    mq(c("UPDATE tmp SET job_def_id = (SELECT job_def_id FROM %s_job_def AS jd WHERE",
         "jd.prob_id = tmp.prob_id AND jd.algo_id = tmp.algo_id AND", 
         "jd.prob_pars = tmp.prob_pars AND jd.algo_pars = tmp.algo_pars)",
         "WHERE tmp.job_def_id IS NULL"), reg$id, con = con)

    message("Writing job status information ...")
    # insert into job status table
    mq(c("INSERT INTO %1$s_job_status(job_def_id, repl)",
         "SELECT cp.job_def_id, cp.repl FROM cp WHERE NOT EXISTS",
         "(SELECT * FROM %1$s_job_status AS js WHERE",
         "cp.job_def_id = js.job_def_id AND cp.repl = js.repl)"),
       reg$id, con = con)

    message("Setting seeds ...")
    # set seed for new jobs to job_id + reg$seed - 1
    mq("UPDATE %s_job_status SET seed = job_id + %i WHERE job_id > %i", 
       reg$id, reg$seed - 1L, max.job.id, con = con)
  })

  if(is.error(ok)) {
    dbRollback(con)
    stopf("Error inserting new experiemts: %s", as.character(ok))
  } else {
    dbCommit(con)
  }


  message("Creating directories ...")
  BatchJobs:::createShardedDirs(reg, getJobIds(reg))
  job.ids = mq("SELECT job_id FROM %s_job_status WHERE job_id > %i", 
               reg$id, max.job.id, con = con)$job_id
  invisible(job.ids)
}
