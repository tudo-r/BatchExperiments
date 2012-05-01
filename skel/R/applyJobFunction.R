#' @method applyJobFunction ExperimentRegistry
#' @S3method applyJobFunction ExperimentRegistry
applyJobFunction.ExperimentRegistry = function(reg, job) {
  # helper function which either returns the already cached static part
  # or loads the static part from the file system.
  # returns a function with its own environment containing cache flags
  staticGetter = function(file.dir, id, cache) {
    static = NULL
    cached = FALSE
    function() {
      message("Getting static problem part %s ...", id)
      if(cache) {
        if (cached)
          return(static)
        static <<- getProblemPart(file.dir, id, "static")
        cached <<- TRUE
        return(static)
      }
      return(getProblemPart(file.dir, id, "static"))
    }
  }

  # first, load the dynamic problem function and the
  # algorithm. As functions, these should be quite small and
  # their formals will determine which tasks next to perform.
  dynamic.fun = getProblemPart(reg$file.dir, job$prob.id, "dynamic")
  algo = loadAlgorithm(reg$file.dir, job$algo.id)$fun

  # create named logical caches for the formals
  prob.use = c("job", "static") %in% names(formals(dynamic.fun))
  names(prob.use) = c("job", "static")
  algo.use = c("job", "static", "dynamic") %in% names(formals(algo))
  names(algo.use) = c("job", "static", "dynamic")

  # if both problem and algorithm rely on the static part we use a
  # cache to avoid reading the file twice. If the static part is only
  # required once we try hard to let lazy evaluation kick in
  if (prob.use["static"] || algo.use["static"])
    static = staticGetter(reg$file.dir, job$prob.id, prob.use["static"] && algo.use["static"])

  if (algo.use["dynamic"]) {
    if(is.null(dynamic.fun)) {
      dynamic = function() NULL
    } else {
      dynamic = function() {
        messagef("Generating problem %s ...", job$prob.id)
        seed = BatchJobs:::seeder(reg, job$prob.seed)
        on.exit(seed$reset())

        # choose an applyDynamic function
        # -> interpret logical use vector as binary number and switch on decimal:
        applyDynamic = switch(as.integer(c(1, 2) %*% prob.use) + 1L,
                              function(...) dynamic.fun(...),
                              function(...) dynamic.fun(job=job, ...),
                              function(...) dynamic.fun(static=static(), ...),
                              function(...) dynamic.fun(job=job, static=static(), ...))
        do.call(applyDynamic, job$prob.pars)
      }
    }
  }

  messagef("Applying Algorithm %s ...", job$algo.id)
  # choose an applyAlgo function
  # -> interpret logical use vector as binary number and switch on decimal:
  applyAlgo = switch(as.integer(c(1, 2, 4) %*% algo.use) + 1L,
                     function(...) algo(...),
                     function(...) algo(job=job, ...),
                     function(...) algo(static=static(), ...),
                     function(...) algo(job=job, static=static(), ...),
                     function(...) algo(dynamic=dynamic(), ...),
                     function(...) algo(job=job, dynamic=dynamic(), ...),
                     function(...) algo(static=static(), dynamic=dynamic(), ...),
                     function(...) algo(job=job, static=static(), dynamic=dynamic(), ...))
  do.call(applyAlgo, job$algo.pars)
}
