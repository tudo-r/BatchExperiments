#' Generate dynamic part of problem.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param id [\code{character(1)}]\cr
#'   Id of job.
#' @return Dynamic part of problem.
#' @export
generateProblemInstance = function(reg, id) {
  job = getJob(reg, id)
  static = getProblemPart(reg$file.dir, id, "static")
  dynamic.fun = getProblemPart(reg$file.dir, job$prob.id, "dynamic")
  prob.use = c("job", "static") %in% names(formals(dynamic.fun))
  names(prob.use) = c("job", "static")
  seed = BatchJobs:::seeder(reg, job$prob.seed)
  on.exit(seed$reset())
  applyDynamic = switch(as.integer(c(1, 2) %*% prob.use) + 1L,
                        function(...) dynamic.fun(...),
                        function(...) dynamic.fun(job=job, ...),
                        function(...) dynamic.fun(static=static(), ...),
                        function(...) dynamic.fun(job=job, static=static(), ...))
  do.call(applyDynamic, job$prob.pars)
}