source("../BatchJobs/skel/R/helpers.R")
library(methods)
library(devtools)
library(testthat)
library(BatchJobs)


if (interactive()) {
  load_all("skel")
} else {
  library(BatchExperiments)  
}
cf = BatchJobs:::makeClusterFunctionsUnitTests()

setBatchJobsConf(cluster.functions=cf,
                 mail.start="none", 
                 mail.done="none", 
                 mail.error="none", 
                 mail.from="bernd_bischl@gmx.net",
                 mail.to="bernd_bischl@gmx.net",
                 mail.control=list(smtpServer="mail"))
  
source("skel/inst/tests/helpers.R")

test_dir("skel/inst/tests")
