library(devtools)
library(testthat)

if (interactive()) {
  library(BBmisc)
  library(fail)
  library(DBI)
  library(sendmailR)
  library(brew)
  library(fail)
  library(RSQLite)
  library(digest)
  library(plyr)

  library(BatchJobs)
  library(plyr)
  load_all(".")
} else {
  library("BatchExperiments")
}

test_dir("tests/testthat")
