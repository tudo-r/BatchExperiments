# BatchExperiments

## Core features
* Extends [BatchJobs](https://github.com/tudo-r/BatchJobs) with functionality required for comprehensive computer experiments
* Abstraction to link algorithms to problems and thereby define computer experiments
* Associate statistical designs with parameters of problems and algorithms
* Support for statistical replications of experiments
* Collect parameters and results into a clearly represented data frame with one simple function call
* Extend your study later with further problems, algorithms, parameters or replications
* Clear separation between all stages: your methods under consideration, experiment defintions and execution layer.
* Readable and succint code for your experiments
* Internally handled seeds guarantee reproducibility


## Quickstart and Documentation
To install the latest stable release from CRAN:
```splus
install.packages("BatchExperiments")
```
To install the development version use [devtools](http://cran.r-project.org/web/packages/devtools):
```splus
library(devtools);
install_github("BatchJobs", username="tudo-r")
install_github("BatchExperiments", username="tudo-r")
```
Please see the documentation of [BatchJobs](https://github.com/tudo-r/BatchJobs) to set up your system for parallel execution.
Currently the best introduction to the package is our [technical report](http://sfb876.tu-dortmund.de/PublicPublicationFiles/bischl_etal_2012a.pdf).
For more detailed information on the functions please use the [R documentation](http://tudo-r.github.io/BatchExperiments/).
You can also peek into some examples provided in the [wiki](../../wiki).

We also have a [mailing list](http://groups.google.com/group/batchjobs).

[![Build Status](https://travis-ci.org/tudo-r/BatchExperiments.png)](https://travis-ci.org/tudo-r/BatchExperiments)
