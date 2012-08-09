#' @method applyJobFunction ExperimentRegistry
#' @S3method applyJobFunction ExperimentRegistry
applyJobFunction.ExperimentRegistry = function(reg, job) {
  algo = loadAlgorithm(reg$file.dir, job$algo.id)$fun
  algo.use = c("job", "static", "dynamic", "stash") %in% names(formals(algo))
  names(algo.use) = c("job", "static", "dynamic", "stash")

  # determine what we need and define getter functions
  if (algo.use["static"])
    static = getStaticLazy(reg, job)
  if (algo.use["dynamic"])
    dynamic = getDynamicLazy(reg, job)
  if (algo.use["stash"])
    stash = getStashObject(reg$file.dir)

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
    function(...) algo(stash=stash, ...),
    function(...) algo(job=job, stash=stash, ...),
    function(...) algo(static=static(), stash=stash, ...),
    function(...) algo(job=job, static=static(), stash=stash, ...),
    function(...) algo(dynamic=dynamic(), stash=stash, ...),
    function(...) algo(job=job, dynamic=dynamic(), stash=stash, ...),
    function(...) algo(static=static(), dynamic=dynamic(), stash=stash, ...),
    function(...) algo(job=job, static=static(), dynamic=dynamic(), stash=stash, ...))

  # IMPORTANT: Note that the order of the messages in the log files can be confusing.
  # This is caused by lazy evaluation, but we cannot live w/o it.
  # Therefore it is possible to get errors on the slave with the last message being
  # "Generating problem[...]", but the actual error is thron in the algorithm
  
  messagef("Applying Algorithm %s ...", job$algo.id)
  do.call(f, job$algo.pars)
}
