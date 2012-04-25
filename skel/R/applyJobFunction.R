#' @method applyJobFunction ExperimentRegistry
#' @S3method applyJobFunction ExperimentRegistry
applyJobFunction.ExperimentRegistry = function(reg, job) {
  # helper function which either returns the already cached static part
  # or loads the static part from the file system.
  getStatic = function(file.dir, id, cache) {
    static = NULL
    cached = FALSE
    function() {
      message("Getting static problem part %s ...", id)
      if(cache) {
        if (cached)
          return(static)
        static <<- BatchExperiments:::getProblemPart(file.dir, id, "static")
        cached <<- TRUE
        return(static)
      }
      return(BatchExperiments:::getProblemPart(file.dir, id, "static"))
    }
  }
  # helper function to apply the dynamic problem function.
  # inside a function for lazy evaluation
  getDynamic = function(reg, job, fun, use.static, static) {
    if (is.null(fun))
      return(fun)
    messagef("Generating problem %s ...", job$prob.id)
    seed = BatchJobs:::seeder(reg, job$prob.seed)

    if (use.static)
      applyDynamic = function(...) fun(static=static, ...)
    else
      applyDynamic = function(...) fun(...)
    dynamic = try(do.call(applyDynamic, job$prob.pars))

    seed$reset()
    return(dynamic)
  }

  # first, load the dynamic problem function and the
  # algorithm. As functions, these should be quite small and
  # their formals will determine which tasks next to perform.
  dynamic.fun = BatchExperiments:::getProblemPart(reg$file.dir, job$prob.id, "dynamic")
  algo = BatchExperiments:::loadAlgorithm(reg$file.dir, job$algo.id)$fun

  # create named logical caches for the formals
  prob.use = "static" %in% names(formals(dynamic.fun))
  names(prob.use) = "static"
  algo.use = c("static", "dynamic") %in% names(formals(algo))
  names(algo.use) = c("static", "dynamic")

  # if both problem and algorithm rely on the static part we use a
  # cache to avoid reading the file twice. If the static part is only
  # required once we try hard to let lazy evaluation kick in
  cache.static = prob.use["static"] && algo.use["static"]
  static = getStatic(reg$file.dir, job$prob.id, prob.use["static"] && algo.use["static"])

  messagef("Applying Algorithm %s ...", job$algo.id)
  # choose an applyAlgo function
  # we have 2 optional formal arguments, 4 possible combinations of formals' existence.
  # -> interpret logical vector as binary number and switch on decimal:
  # use.static * 2^0 + use.dynamic * 2^1
  sw = sum(1:2 * algo.use)
  applyAlgo = switch(sw + 1L, # switch on numerics is really weird in R ...
                     function(...) algo(...),
                     function(...) algo(static=static(), ...),
                     function(...) algo(dynamic=getDynamic(reg, job, dynamic.fun, prob.use["static"], static()), ...),
                     function(...) algo(static=static(), dynamic=getDynamic(reg, job, dynamic.fun, prob.use["static"], static()), ...))
  do.call(applyAlgo, job$algo.pars)
}
