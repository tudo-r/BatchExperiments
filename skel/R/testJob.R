#' @S3method copyRequiredJobFiles ExperimentRegistry
copyRequiredJobFiles.ExperimentRegistry = function(reg1, reg2, id) {
  job = getJob(reg1, id, check.id=FALSE)
  src = getProblemFilePath(reg1$file.dir, job$prob.id)
  dest = getProblemFilePath(reg2$file.dir, job$prob.id)
  message("Copying problem file: ", src)
  file.copy(src, dest)
  src = getAlgorithmFilePath(reg1$file.dir, job$algo.id)
  dest = getAlgorithmFilePath(reg2$file.dir, job$algo.id)
  message("Copying algorithm file: ", src)
  file.copy(src, dest)
}
