#' @S3method setJobFunction ExperimentRegistry
setJobFunction.ExperimentRegistry = function(reg, ids, fun, more.args=list()) {
  stop("setJobFunction not available for BatchExperiments. Please use addProblem or addAlgorithm with overwrite=TRUE")
}
