#' ExperimentJob
#'
#' You can access job properties using the \code{job} object which is optionally passed
#' to dynamic problem functions and algorithms. The object is a named list with the following
#' elements:
#' \describe{
#'   \item{\code{id}:}{Job ID as integer(1)}
#'   \item{\code{prob.id}:}{Problem ID as character(1)}
#'   \item{\code{id}:}{algo.id}{Algorithm ID as name/string}
#'   \item{\code{prob.pars}:}{Problem parameters as named list}
#'   \item{\code{algo.ids}:}{Algorithm parameters as named list}
#'   \item{\code{seed}:}{Seed set right before algorithm execution}
#'   \item{\code{prob.seed}:}{Seed set right before generation of problem instance}
#'   \item{\code{repl}:}{Replication number of this experiment}
#' }
#' @name ExperimentJob
#' @rdname ExperimentJob
NULL

makeExperimentJob = function(id=NA_integer_, prob.id, prob.pars, algo.id, algo.pars, repl, seed, prob.seed) {
  setClasses(list(id=id, prob.id=prob.id, prob.pars=prob.pars, algo.id=algo.id,
                  algo.pars=algo.pars, repl=repl, seed=seed, prob.seed=prob.seed),
             c("ExperimentJob", "Job"))
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
  cat("  Problem seed:", x$prob.seed, "\n")
}
