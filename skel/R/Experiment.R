# Creates and experiment in which an algorithm is applied to a problem.
#
# Every object is a list that contains the passed arguments of the constructor.
# @param id [\code{integer(1)}]\cr
#   Job id, determined by DB autoincrement.
#   Default is \code{as.integer(NA)}.
# @aliases Experiment
makeExperimentJob = function(id=as.integer(NA), prob.id, prob.pars, algo.id, algo.pars, repl, seed) {
  structure(list(id=id, prob.id=prob.id, prob.pars=prob.pars, 
    algo.id=algo.id, algo.pars=algo.pars, repl=repl, seed=seed), 
    class=c("ExperimentJob", "Job"))
}

makeReplicatedExperiment = function(id=as.integer(NA), prob.id, prob.pars, algo.id, algo.pars, repls) {
  structure(list(id=id, prob.id=prob.id, prob.pars=prob.pars, 
    algo.id=algo.id, algo.pars=algo.pars, repls=repls), class="ReplicatedExperiment")
}




#' @S3method print ExperimentJob
print.ExperimentJob = function(x, ...) {
  cat("Experiment:", "\n")
  cat("  Problem:", x$prob.id, "\n")
  cat("  Problem parameters:", listToShortString(x$prob.pars), "\n")
  cat("  Algorithm:", x$algo.id, "\n")
  cat("  Algorithm parameters:", listToShortString(x$algo.pars), "\n")
  cat("  Replication:", x$repl, "\n")
  cat("  Seed:", x$seed, "\n")
}


