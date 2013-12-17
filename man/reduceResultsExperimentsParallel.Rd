\name{reduceResultsExperimentsParallel}
\alias{reduceResultsExperimentsParallel}
\title{Reduce very many results in parallel.}
\usage{
reduceResultsExperimentsParallel(reg, ids, part = as.character(NA), fun, ...,
  njobs = 20L, strings.as.factors = default.stringsAsFactors())
}
\arguments{
  \item{reg}{[\code{\link{ExperimentRegistry}}]\cr
  Registry.}

  \item{ids}{[\code{integer}]\cr Ids of selected
  experiments.  Default is all jobs for which results are
  available.}

  \item{part}{[\code{character}] Only useful for multiple
  result files, then defines which result file part(s)
  should be loaded.  \code{NA} means all parts are loaded,
  which is the default.}

  \item{fun}{[\code{function(job, res, ...)}]\cr Function
  to collect values from \code{job} and result \code{res}
  object, the latter from stored result file.  Must return
  an object which can be coerced to a \code{data.frame}
  (e.g. a \code{list}).  Default is a function that simply
  returns \code{res} which may or may not work, depending
  on the type of \code{res}.}

  \item{...}{[any]\cr Additional arguments to \code{fun}.}

  \item{njobs}{[\code{integer(1)}] Number of parallel jobs
  to create.  Default is 20.}

  \item{strings.as.factors}{[\code{logical(1)}] Should all
  character columns in result be converted to factors?
  Default is \code{default.stringsAsFactors()}.}
}
\value{
[\code{data.frame}]. Aggregated results, containing problem
and algorithm paramaters and collected values.
}
\description{
Basically the same as
\code{\link{reduceResultsExperiments}} but creates a few
(hopefully short) jobs to reduce the results in parallel.
The function internally calls \code{\link{batchMapQuick}},
does \dQuote{busy-waiting} till all jobs are done and
cleans all temporary files up. Useful when you have very
results and reducing is slow.
}

