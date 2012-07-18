#' @method applyJobFunction ExperimentRegistry
#' @S3method applyJobFunction ExperimentRegistry
applyJobFunction.ExperimentRegistry = function(reg, job) {
  getStash = function(file.dir, items) {
    function() sapply(items, getStashed, file.dir = file.dir, simplify=FALSE)
  }

  getStatic = function(file.dir, id) {
    function() getProblemPart(file.dir, id, "static")
  }

  getDynamic = function(prob.use) {
    # we avoid copies and let lazy evaluation kick in if the specific parts are not
    # needed. Seems a bit cumbersome, but worth it
    f = switch(sum(c(1L, 2L, 4L)[prob.use]) + 1L,
      function(...) dynamic.fun(...),
      function(...) dynamic.fun(job=job, ...),
      function(...) dynamic.fun(static=static(), ...),
      function(...) dynamic.fun(job=job, static=static(), ...),
      function(...) dynamic.fun(stash=prob.stash(), ...),
      function(...) dynamic.fun(job=job, stash=prob.stash(), ...),
      function(...) dynamic.fun(static=static(), stash=prob.stash(), ...),
      function(...) dynamic.fun(job=job, static=static(), stash=prob.stash(), ...))

    function() {
      messagef("Generating problem %s ...", job$prob.id)
      seed = BatchJobs:::seeder(reg, job$prob.seed)
      on.exit(seed$reset())
      do.call(f, job$prob.pars[setdiff(names(job$prob.pars), "stash")])
    }
  }

  # first, load the dynamic problem function and the
  # algorithm. As functions, these should be quite small and
  # their formals will determine which tasks next to perform.
  dynamic.fun = getProblemPart(reg$file.dir, job$prob.id, "dynamic")
  algo = loadAlgorithm(reg$file.dir, job$algo.id)$fun

  # create named logical caches for the formals
  prob.use = c("job", "static", "stash") %in% names(formals(dynamic.fun))
  names(prob.use) = c("job", "static", "stash")
  algo.use = c("job", "static", "dynamic", "stash") %in% names(formals(algo))
  names(algo.use) = c("job", "static", "dynamic", "stash")

  # determine what we need and define getter functions
  if (prob.use["stash"])
    prob.stash = getStash(reg$file.dir, job$prob.pars$stash)
  if (algo.use["stash"])
    algo.stash = getStash(reg$file.dir, job$algo.pars$stash)
  if (prob.use["static"] || algo.use["static"])
    static = getStatic(reg$file.dir, job$prob.id)
  if (algo.use["dynamic"]) {
    if (is.null(dynamic.fun))
      dynamic = function() NULL
    else
      dynamic = getDynamic(prob.use)
  }


  # IMPORTANT: Note that the order of the messages in the log files can be confusing.
  # This is caused by lazy evaluation, but we cannot live w/o it.
  # Therefore it is possible to get errors on the slave with the last message being
  # "Generating problem[...]", but the actual error is thron in the algorithm
  messagef("Applying Algorithm %s ...", job$algo.id)

  # switch on algo formals and apply algorithm function
  f = switch(sum(c(1L, 2L, 4L, 8L)[algo.use]) + 1L,
    function(...) algo(...),
    function(...) algo(job=job, ...),
    function(...) algo(static=static(), ...),
    function(...) algo(job=job, static=static(), ...),
    function(...) algo(dynamic=dynamic(), ...),
    function(...) algo(job=job, dynamic=dynamic(), ...),
    function(...) algo(static=static(), dynamic=dynamic(), ...),
    function(...) algo(job=job, static=static(), dynamic=dynamic(), ...),
    function(...) algo(stash=algo.stash(), ...),
    function(...) algo(job=job, stash=algo.stash(), ...),
    function(...) algo(static=static(), stash=algo.stash(), ...),
    function(...) algo(job=job, static=static(), stash=algo.stash(), ...),
    function(...) algo(dynamic=dynamic(), stash=algo.stash(), ...),
    function(...) algo(job=job, dynamic=dynamic(), stash=algo.stash(), ...),
    function(...) algo(static=static(), dynamic=dynamic(), stash=algo.stash(), ...),
    function(...) algo(job=job, static=static(), dynamic=dynamic(), stash=algo.stash(), ...))

  do.call(f, job$algo.pars[setdiff(names(job$algo.pars), "stash")])
}
