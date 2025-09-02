#' @title The BatchExperiments package
#'
#' @description
#' Extends the BatchJobs package to run statistical experiments on
#' batch computing clusters.
#'
#' @section Additional information:
#'
#' \describe{
#'   \item{Homepage:}{\url{https://github.com/tudo-r/BatchExperiments}}
#'   \item{Wiki:}{\url{https://github.com/tudo-r/BatchExperiments/wiki}}
#' }
#'
#' @name BatchExperiments
#' @import checkmate
#' @import BBmisc
#' @import DBI
#' @import RSQLite
#' @import BatchJobs
#' @import data.table
#' @importFrom stats setNames na.omit
#' @importFrom utils head capture.output packageVersion
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  backports::import(pkgname)
}

addIntModulo = getFromNamespace("addIntModulo", "BatchJobs")
buffer = getFromNamespace("buffer", "BatchJobs")
checkDir = getFromNamespace("checkDir", "BatchJobs")
checkPart = getFromNamespace("checkPart", "BatchJobs")
createShardedDirs = getFromNamespace("createShardedDirs", "BatchJobs")
dbConnectToJobsDB = getFromNamespace("dbConnectToJobsDB", "BatchJobs")
dbCreateJobStatusTable = getFromNamespace("dbCreateJobStatusTable", "BatchJobs")
dbConnectToJobsDB = getFromNamespace("dbConnectToJobsDB", "BatchJobs")
dbCreateJobStatusTable = getFromNamespace("dbCreateJobStatusTable", "BatchJobs")
getJobInfoInternal = getFromNamespace("getJobInfoInternal", "BatchJobs")
getRandomSeed = getFromNamespace("getRandomSeed", "BatchJobs")
getResult = getFromNamespace("getResult", "BatchJobs")
isRegistryDir = getFromNamespace("isRegistryDir", "BatchJobs")
makeRegistryInternal = getFromNamespace("makeRegistryInternal", "BatchJobs")
saveRegistry = getFromNamespace("saveRegistry", "BatchJobs")
seeder = getFromNamespace("seeder", "BatchJobs")
