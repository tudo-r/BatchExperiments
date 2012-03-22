#!/usr/bin/env Rscript
required_packages <- c("plyr")

installed_packages <- rownames(installed.packages())

# This one is special because the user might also want to install it from SVN
if(! "BatchJobs" %in% installed_packages)
  stop("BatchJobs not installed. Please install it first!")

missing_packages <- setdiff(required_packages, installed_packages)
if (length(missing_packages) > 0) {
  message("Installing the following missing packages:")
  print(missing_packages)
  install.packages(missing_packages,
                   repos="http://cran.at.r-project.org")
} else {
  message("All dependencies installed.")
}
