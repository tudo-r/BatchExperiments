info = function(...) {
  if (getOption("BatchJobs.verbose", default=TRUE))
    message(sprintf(...))
}
