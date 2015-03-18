# BatchExperiments

[![Build Status](https://travis-ci.org/tudo-r/BatchExperiments.png)](https://travis-ci.org/tudo-r/BatchExperiments)
[![Build status](https://ci.appveyor.com/api/projects/status/f9052xe5y3njcudd/branch/master?svg=true)](https://ci.appveyor.com/project/mllg/batchexperiments/branch/master)
[![Coverage Status](https://coveralls.io/repos/tudo-r/BatchExperiments/badge.svg)](https://coveralls.io/r/tudo-r/BatchExperiments)

## If you use the package, please cite it
  ```

To cite BatchJobs or BatchExperiments in publications use:

  Bernd Bischl, Michel Lang, Olaf Mersmann, Joerg Rahnenfuehrer, Claus Weihs (2015). BatchJobs and BatchExperiments: Abstraction Mechanisms for Using R in Batch Environments. Journal
  of Statistical Software, 64(11), 1-25. URL http://www.jstatsoft.org/v64/i11/.

A BibTeX entry for LaTeX users is

  @Article{,
    title = {{BatchJobs} and {BatchExperiments}: Abstraction Mechanisms for Using {R} in Batch Environments},
    author = {Bernd Bischl and Michel Lang and Olaf Mersmann and J{\"o}rg Rahnenf{\"u}hrer and Claus Weihs},
    journal = {Journal of Statistical Software},
    year = {2015},
    volume = {64},
    number = {11},
    pages = {1--25},
    url = {http://www.jstatsoft.org/v64/i11/},
  }
  ```

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
install_github("tudo-r/BatchJobs")
install_github("tudo-r/BatchExperiments")
```
Please see the documentation of [BatchJobs](https://github.com/tudo-r/BatchJobs) to set up your system for parallel execution.
Currently the best introduction to the package is our [technical report](http://sfb876.tu-dortmund.de/PublicPublicationFiles/bischl_etal_2012a.pdf).
For more detailed information on the functions please use the [R documentation](http://tudo-r.github.io/BatchExperiments/).
You can also peek into some examples provided in the [wiki](../../wiki).

We also have a [mailing list](http://groups.google.com/group/batchjobs).

