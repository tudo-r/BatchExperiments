makeProblem = function(id, static, dynamic) {
  structure(list(id=id, static=static, dynamic=dynamic),
            class="Problem")
}

#' Add a algorithm to problem and stores it on disk.
#' @title Add a problem to registry.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param id [\code{character(1)}]\cr
#'   Name of problem.
#' @param static [any]\cr
#'   Static part of problem that never changes and is not dependent on parameters.
#'   Default is \code{NULL}.
#' @param dynamic [\code{function(static, ...)}]\cr
#'   R generator function that creates dynamic / stochastic part of problem instance, which might be dependent on parameters.
#'   First parameter is static problem part \code{static}, further parameters from design are passed to ... argument
#'   on instance creation time.
#'   Default is \code{NULL}.
#' @param seed [\code{integer(1)}]\cr
#'   Start seed for this problem. This allows the \dQuote{synchronization} of a stochastic
#'   problem across algorithms, so that different algorithms are evaluated on the same stochastic instance.
#'   The seeding mechanism works as follows, if a problem seed is defined:
#'   (1) Before the dynamic part of a problem is instantiated,
#'   the seed of the problem + replication - 1 is set, so for the first
#'   replication the exact problem seed is used. (2) The stochastic part of the problem is
#'   instantiated (3) From now on the usual experiment seed of the registry is used,
#'   see \code{\link{ExperimentRegistry}}.
#'   If \code{seed} is set to \code{NULL} this extra problem seeding is switched off, meaning
#'   different algorithms see different stochastic versions of the same problem.
#'   Default is \code{NULL}.
#' @param overwrite [\code{logical(1)}]\cr
#'   Overwrite the problem file if it already exists?
#'   Default is \code{FALSE}.
#' @return [\code{character(1)}]. Invisibly returns the id.
#' @aliases Problem
#' @examples \dontrun{
#' # two simple test functions:
#' testfun1 = function(x) sum(x^2)
#' testfun2 = function(x) -exp(-sum(abs(x)))
#' 
#' reg = makeExperimentRegistry("BatchExample", seed=123)
#' # the functions are not parameterized and are not stochastic, so they are declared static:
#' addProblem(reg, "testfun1", static=testfun1)
#' addProblem(reg, "testfun2", static=testfun2)
#' }
#' @export
addProblem = function(reg, id, static=NULL, dynamic=NULL, seed=NULL, overwrite=FALSE)  {
  checkArg(reg, "ExperimentRegistry")
  checkArg(id, cl = "character", len = 1L, na.ok = FALSE)
  BatchJobs:::checkIdValid(id)
  if (is.null(static) && is.null(dynamic))
    stop("One of args 'static' or 'dynamic' must not be NULL!")
  if (!is.null(dynamic))
    checkArg(dynamic, formals = "static")
  if (id %in% getAlgorithmIds(reg))
    stopf("Algorithm with same id as your problem already exists: %s", id)
  if (!overwrite && id %in% getProblemIds(reg))
    stopf("Problem with same id already exists and overwrite=FALSE: %s", id)

  problem = makeProblem(id, static, dynamic)
  fn = getProblemFilePath(reg$file.dir, id)
  message("Writing problem file: ", fn)
  save(file=fn, problem)
  dbAddProblem(reg, id, seed)
  invisible(id)
}

#' @S3method print Problem
print.Problem = function(x, ...) {
  cat("Problem:", x$id, "\n")
}

loadProblem = function(file.dir, id) {
  ee = new.env()
  fn = getProblemFilePath(file.dir, id)
  message("Loading problem file: ", fn)
  load(file=fn, envir=ee)
  return(ee$problem)
}
