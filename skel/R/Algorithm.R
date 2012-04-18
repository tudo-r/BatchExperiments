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
#' @param fun [\code{function(static, dynamic, ...)}]\cr
#'   Function which applies the algorithm to a problem instance.
#'   Has to take the the static and dynamic part of a problem instance as first two arguments.
#'   Further parameters from design are passed to ... argument.
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
  checkArg(fun, formals=c("static", "dynamic"))
  checkArg(overwrite, "logical", len=1L, na.ok=FALSE)

  if (id %in% dbGetProblemIds(reg))
    stopf("Problem with same id as your algorithm already exists: %s", id)
  if (!overwrite && id %in% dbGetAlgorithmIds(reg))
    stopf("Algorithm with same id already exists and overwrite=FALSE: %s", id)

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
  message("Loading algorithm file: ", fn)
  BatchJobs:::loadSingleObject(fn, "algorithm")
}
