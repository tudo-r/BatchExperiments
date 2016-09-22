#' The BatchExperiments package
#'
#' @section Additional information:
#'
#' \describe{
#'   \item{Homepage:}{\url{https://github.com/tudo-r/BatchExperiments}}
#'   \item{Wiki:}{\url{https://github.com/tudo-r/BatchExperiments/wiki}}
#' }
#'
#' @docType package
#' @name BatchExperiments
#' @import checkmate
#' @import BBmisc
#' @import DBI
#' @import RSQLite
#' @import BatchJobs
#' @import data.table
#' @importFrom stats setNames na.omit
#' @importFrom utils head capture.output packageVersion
NULL


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
