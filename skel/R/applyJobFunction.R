#' @method applyJobFunction ExperimentRegistry
#' @S3method applyJobFunction ExperimentRegistry
applyJobFunction.ExperimentRegistry = function(reg, job) {
  message("Loading problem: ", job$prob.id)
  prob = loadProblem(reg$file.dir, job$prob.id)
  message("Generating problem ", job$prob.id, "...")
  message("Static problem part:")
  message(capture.output(str(prob$static, max.level=1L, list.len=5L)))
  if (!is.null(prob$dynamic)) {
    if (!is.null(prob$seed)) {
      cur.seed = get(".Random.seed", envir = .GlobalEnv)
      seed = prob$seed + job$repl - 1L
      message("Setting problem seed: ", seed)
      set.seed(seed)
    }
    prob.dynamic = try(do.call(prob$dynamic, c(list(static = prob$static), job$prob.pars)))
    if (!is.null(prob$seed)) {
      message("Switching back to previous seed.")
      assign(".Random.seed", cur.seed, envir = .GlobalEnv)
    }
  } else {
    prob.dynamic = NULL
  }
  message("Dynamic problem part:")
  message(capture.output(str(prob.dynamic, max.level=1L, list.len=5L)))
  message("Loading algorithm: ", job$algo.id)
  algo = loadAlgorithm(reg$file.dir, job$algo.id)
  message("Applying algorithm ", job$algo.id, "...")
  args = c(list(static = prob$static, dynamic = prob.dynamic), job$algo.pars)
  do.call(algo$fun, args)
}
