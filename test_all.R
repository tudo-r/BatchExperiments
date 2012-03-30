source("skel/inst/tests/helpers.R")
library(methods)
library(devtools)
library(testthat)
library(BatchJobs)
conf = BatchJobs:::getBatchJobsConf()
conf$cluster.functions = BatchJobs:::makeClusterFunctionsUnitTests()
conf$mail.start = "none"
conf$mail.done = "none"
conf$mail.error = "none"

if (interactive()) {
  load_all("skel")
} else {
  library("BatchExperiments")  
}

test_dir("skel/inst/tests")
print(ls(all=TRUE))
