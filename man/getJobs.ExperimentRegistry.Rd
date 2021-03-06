% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getJob.R
\name{getJobs.ExperimentRegistry}
\alias{getJobs.ExperimentRegistry}
\title{Get jobs (here: experiments) from registry by id.}
\usage{
\method{getJobs}{ExperimentRegistry}(reg, ids, check.ids = TRUE)
}
\arguments{
\item{reg}{[\code{\link{ExperimentRegistry}}]\cr
Registry.}

\item{ids}{[\code{integer}]\cr
Ids of job.
Default is all jobs.}

\item{check.ids}{[\code{logical(1)}]\cr
Check the job ids?
Default is \code{TRUE}.}
}
\value{
[list of \code{Experiment}].
}
\description{
Constructs an \code{\link{Experiment}} for each job id provided.
}
\seealso{
Other get: 
\code{\link{getAlgorithmIds}()},
\code{\link{getAlgorithm}()},
\code{\link{getExperimentParts}()},
\code{\link{getProblemIds}()},
\code{\link{getProblem}()}
}
\concept{get}
