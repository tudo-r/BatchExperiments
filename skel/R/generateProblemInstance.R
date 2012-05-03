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
  dynamic.fun = getProblemPart(reg$file.dir, job$prob.id, "dynamic")
  prob.use = c("job", "static") %in% names(formals(dynamic.fun))
  applyDynamic = switch(as.integer(c(1, 2) %*% prob.use) + 1L,
                        function(...) dynamic.fun(...),
                        function(...) dynamic.fun(job=job, ...),
                        function(...) dynamic.fun(static=getProblemPart(reg$file.dir, id, "static"), ...),
                        function(...) dynamic.fun(job=job, static=getProblemPart(reg$file.dir, id, "static"), ...))
  seed = BatchJobs:::seeder(reg, job$prob.seed)
  on.exit(seed$reset())
  do.call(applyDynamic, job$prob.pars)
}
