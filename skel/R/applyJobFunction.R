#' @method applyJobFunction ExperimentRegistry
#' @S3method applyJobFunction ExperimentRegistry
applyJobFunction.ExperimentRegistry = function(reg, job) {
  message("Loading problem: ", job$prob.id)
  prob = loadProblem(reg, job$prob.id, seed=FALSE)

  message("Generating problem ", job$prob.id, "...")
  message("Static problem part:")
  message(capture.output(str(prob$static, max.level=1L, list.len=5L)))

  if (!is.null(prob$dynamic)) {
    message("Setting problem seed: ", job$prob.seed)
    seed = BatchJobs:::seeder(reg, job$prob.seed)
    prob$dynamic = try(do.call(prob$dynamic, c(list(static = prob$static), job$prob.pars)))
    message("Switching back to previous seed.")
    seed$reset()
  }

  message("Dynamic problem part:")
  message(capture.output(str(prob$dynamic, max.level=1L, list.len=5L)))

  message("Loading algorithm: ", job$algo.id)
  algo = loadAlgorithm(reg$file.dir, job$algo.id)

  message("Applying algorithm ", job$algo.id, "...")
  do.call(algo$fun, c(list(static = prob$static, dynamic = prob$dynamic), job$algo.pars))
}
