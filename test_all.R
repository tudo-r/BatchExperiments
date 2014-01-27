library(methods)
library(devtools)
library(testthat)
library(BatchJobs)
source("inst/tests/helpers.R")
options(BBmisc.ProgressBar.style = "off")
conf = BatchJobs:::getBatchJobsConf()
conf$cluster.functions = BatchJobs:::makeClusterFunctionsUnitTests()
conf$mail.start = "none"
conf$mail.done = "none"
conf$mail.error = "none"

if (interactive()) {
  library("RSQLite")
  library("plyr")
  load_all(".")
} else {
  library("BatchExperiments")
}

test_dir("inst/tests")
