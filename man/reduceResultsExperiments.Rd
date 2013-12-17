\name{reduceResultsExperiments}
\alias{ReducedResultsExperiments}
\alias{reduceResultsExperiments}
\title{Reduce results into a data.frame with all relevant information.}
\usage{
reduceResultsExperiments(reg, ids, part = NA_character_, fun, ...,
  strings.as.factors = default.stringsAsFactors(), block.size, impute.val)
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

  \item{strings.as.factors}{[\code{logical(1)}] Should all
  character columns in result be converted to factors?
  Default is \code{default.stringsAsFactors()}.}

  \item{block.size}{[\code{integer(1)}] Results will be
  fetched in blocks of this size.  Default is max(100, 5
  percent of ids).}

  \item{impute.val}{[\code{named list}]\cr If not missing,
  the value of \code{impute.val} is used as a replacement
  for the return value of function \code{fun} on missing
  results. An empty list is allowed.}
}
\value{
[\code{data.frame}]. Aggregated results, containing problem
and algorithm paramaters and collected values.
}
\description{
Generates a \code{data.frame} with one row per job id. The
columns are: ids of problem and algorithm (named
\dQuote{prob} and \dQuote{algo}), one column per parameter
of problem or algorithm (named by the parameter name), the
replication number (named \dQuote{repl}) and all columns
defined in the function to collect the values. Note that
you cannot rely on the order of the columns. If a paramater
does not have a setting for a certain job / experiment it
is set to \code{NA}. Have a look at
\code{\link{getResultVars}} if you want to use somethink
like \code{\link{ddply}} on the results.
}

