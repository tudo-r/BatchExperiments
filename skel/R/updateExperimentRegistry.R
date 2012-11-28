#' @method updateRegistry ExperimentRegistry
#' @S3method updateRegistry ExperimentRegistry
updateRegistry.ExperimentRegistry = function(reg) {
  # update the BatchJobs part first
  # FIXME NextMethod() ?
  updated = NextMethod()
  #updated = BatchJobs:::.Registry(reg)
  is.updated = !isFALSE(updated)

  version.reg = reg$packages$BatchExperiments$version
  version.pkg = packageVersion("BatchExperiments")
  if (!is.updated && version.reg == version.pkg)
    return(FALSE)
  if (version.reg > version.pkg) {
    warningf("The registry has been used with BatchExperiments version %s, installed is version %s. You should update BatchExperiments on this machine.",
             version.reg, version.pkg)
    if (!is.updated)
      return(FALSE)

    # return registry which is updated by BatchJobs
    return(reg)
  }

  if (version.reg < package_version("1.0.767")) {
    # FIXME fix problem and algorithm file names for windows compability
    path = file.path(reg$file.dir, "problems")
    src = list.files(path, full.names = TRUE, pattern="_static\\.RData$")
    ids = sub("\\.RData$", "", basename(src))
    dest = vapply(ids, function(id) getProblemFilePaths(reg$file.dir, id)["static"], character(1L))
    file.rename(src, dest)

    path = file.path(reg$file.dir, "problems")
    src = list.files(path, full.names = TRUE, pattern="_dynamic\\.RData$")
    ids = sub("\\.RData$", "", basename(src))
    dest = vapply(ids, function(id) getProblemFilePaths(reg$file.dir, id)["static"], character(1L))
    file.rename(src, dest)

    path = file.path(reg$file.dir, "algorithms")
    src = list.files(path, full.names = TRUE)
    ids = sub("\\.RData$", "", basename(src))
    dest = getAlgorithmFilePath(reg$file.dir, ids)
    file.rename(src, dest)
  }

  reg$packages$BatchExperiments$version = version.pkg
  reg
}
