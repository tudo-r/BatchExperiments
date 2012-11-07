#' @method updateRegistry ExperimentRegistry
#' @S3method updateRegistry ExperimentRegistry
updateRegistry.ExperimentRegistry = function(reg) {
  version.reg = reg$packages$BatchExperiments$version
  version.pkg = packageVersion("BatchExperiments")

  # update the BatchJobs part first
  # FIXME NextMethod() ?
  updated = BatchJobs:::updateRegistry.Registry(reg)
  is.updated = !BatchJobs:::isFALSE(updated)

  if (!is.updated && version.reg == version.pkg)
    return(FALSE)
  if (version.reg > version.pkg) {
    warningf("The registry has been used with BatchExperiments version %s, installed is version %s. You should update BatchExperiments on this machine.",
             version.reg, version.pkg)
    if (!is.updated)
      return(FALSE)
  }

  # update registry
  message("Updating ExperimentRegistry and DB to newer version.")
  if (version.reg < package.version("1.0.630")) {
    checkDir(getStashPath(reg$file.dir), create=TRUE)
  }

  reg$packages$BatchExperiments$version = version.pkg
  reg
}
