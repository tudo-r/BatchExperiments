# BatchExperiments

## Core features of BatchExperiments
* Extends [BatchJobs](https://github.com/tudo-r/BatchJobs) with functionality required for comprehensive computer experiments
* Link algorithms to problem instances to define batch jobs and submit them to the (batch) system
* Associate statistical designs with parameters of problems and algorithms
* Statistical replications of experiments
* Collect parameters and results into a clearly represented data frame with one simple function call
* Extend your study later with further problems, algorithms, parameters or replications
* Clear separation between all stages: your methods under consideration, experiment defintions and execution layer
* Write elegant, readable, succint code for your experiments


## Quickstart and Documentation
To install the latest stable release from CRAN:
```splus
install.packages("BatchExperiments")
```
You can install the development version using the [devtools](http://cran.r-project.org/web/packages/devtools) package by executing
```splus
library(devtools); install_github("BatchExperiments", username="tudo-r")
```
Please see the documentation of [BatchJobs](https://github.com/tudo-r/BatchJobs) to set up your cluster.
Currently the best introduction to the package is our [technical report](http://sfb876.tu-dortmund.de/PublicPublicationFiles/bischl_etal_2012a.pdf).
For more detailed information on the functions please use the [R documentation](http://tudo-r.github.io/BatchExperiments/).

We also have a [mailing list](http://groups.google.com/group/batchjobs).

[![Build Status](https://travis-ci.org/tudo-r/BatchExperiments.png)](https://travis-ci.org/tudo-r/BatchExperiments)
