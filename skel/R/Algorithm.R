makeAlgorithm = function(id, fun) {
  structure(list(id=id, fun=fun), class="Algorithm")
}

#' Add an algorithm to registry.
#'
#' Add an algorithm to registry and stores it on disk.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param id [\code{character(1)}]\cr
#'   Name of algorithm.
#' @param fun [\code{function(job, static, dynamic, ...)}]\cr
#'   Function which applies the algorithm to a problem instance.
#'   Takes a \link{Job} object, the \code{static} problem part and the evaluated \code{dynamic}
#'   problem part as arguments.
#'   You may omit any of \code{job}, \code{static} or \code{dynamic}.
#'   In this case, the respective arguments will not get passed to \code{fun}.
#'   Further parameters from \link{Design} are passed to ... argument.
#'   If you are using multiple result files this function must return a named list.
#' @param overwrite [\code{logical(1)}]\cr
#'   Overwrite the algorithm file if it already exists?
#'   Default is \code{FALSE}.
#' @return [\code{character(1)}]. Invisibly returns the id.
#' @aliases Algorithm
#' @export
addAlgorithm = function(reg, id, fun, overwrite=FALSE)  {
  checkArg(reg, "ExperimentRegistry")
  checkArg(id, cl = "character", len=1L, na.ok=FALSE)
  BatchJobs:::checkIdValid(id)
  checkArg(overwrite, "logical", len=1L, na.ok=FALSE)

  if (id %in% dbGetProblemIds(reg))
    stopf("Problem with same id as your algorithm already exists: %s", id)
  if (!overwrite && id %in% dbGetAlgorithmIds(reg))
    stopf("Algorithm with same id already exists and overwrite=FALSE: %s", id)
  
  environment(fun) = emptyenv()
  algorithm = makeAlgorithm(id, fun)
  fn = getAlgorithmFilePath(reg$file.dir, id)
  message("Writing algorithm file: ", fn)
  save(file=fn, algorithm)
  dbAddAlgorithm(reg, id)
  invisible(id)
}

#' @S3method print Algorithm
print.Algorithm = function(x, ...) {
  cat("Algorithm:", x$id, "\n")
}

loadAlgorithm = function(file.dir, id) {
  fn = getAlgorithmFilePath(file.dir, id)
  load2(fn, "algorithm")
}
