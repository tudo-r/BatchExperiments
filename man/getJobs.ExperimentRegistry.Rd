\name{getJobs.ExperimentRegistry}
\alias{getJobs.ExperimentRegistry}
\title{Get jobs (here: experiments) from registry by id.}
\usage{
\method{getJobs}{ExperimentRegistry}(reg, ids, check.ids = TRUE)
}
\arguments{
  \item{reg}{[\code{\link{ExperimentRegistry}}]\cr
  Registry.}

  \item{ids}{[\code{integer}]\cr Ids of job.  Default is
  all jobs.}

  \item{check.ids}{[\code{logical(1)}]\cr Check the job
  ids?  Default is \code{TRUE}.}
}
\value{
[list of \code{Experiment}].
}
\description{
Get jobs (here: experiments) from registry by id.
}

