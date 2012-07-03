#' Summarize selected experiments.
#'
#' A data.frame is returned that contains summary information
#' about the selected experiments. The data.frame is constructed
#' by building the columns \dQuote{prob, <prob.pars>, algo, <algo.pars>, repl},
#' \code{\link{rbind.fill}} is used to connect the rows, so if some parameters do not appear
#  in some experiments, they will be set to \code{NA}.
#' Now only the columns in \code{show} will be selected, how many of such experiments
#' exist will be counted in a new column \dQuote{.count}.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Selected experiments.
#'   Default is all experiments.
#' @param show [\code{character}]\cr
#'   Should detailed information for each single experiment be printed?
#'   Default is \code{c("prob", "algo")}.
#' @return [\code{data.frame}].
#' @export
#' @examples
#' reg <- makeExperimentRegistry("summarizeExperiments", seed=123, file.dir=tempfile())
#' p1 <- addProblem(reg, "p1", static=1)
#' a1 <- addAlgorithm(reg, id="a1", fun=function(static, dynamic, alpha, beta) 1)
#' a2 <- addAlgorithm(reg, id="a2", fun=function(static, dynamic, alpha, gamma) 2)
#' ad1 <- makeDesign(a1, exhaustive = list(alpha=1:2, beta=1:2))
#' ad2 <- makeDesign(a2, exhaustive = list(alpha=1:2, gamma=7:8))
#' addExperiments(reg, algo.designs=list(ad1, ad2), repls=2)
#' print(summarizeExperiments(reg))
#' print(summarizeExperiments(reg, show=c("prob", "algo", "alpha", "gamma")))
summarizeExperiments = function(reg, ids, show=c("prob", "algo")) {
  checkArg(reg, "ExperimentRegistry")
  checkArg(show, "character", min.len=1, na.ok=FALSE)
  # FIXME: the following code is still a bit slow
  # but we gain much by handling repls in the DB
  rexps = dbGetReplicatedExperiments(reg, ids)
  prob = do.call(rbind.fill, lapply(rexps, function(e)
    if (length(e$prob.pars) == 0L)
      data.frame(prob=e$prob.id)
    else
      cbind(prob=e$prob.id, as.data.frame(e$prob.pars))
  ))
  algo = do.call(rbind.fill, lapply(rexps, function(e)
    if (length(e$algo.pars) == 0L)
      data.frame(algo=e$algo.id)
    else
      cbind(algo=e$algo.id, as.data.frame(e$algo.pars))
  ))
  repls = extractSubList(rexps, "repls", integer(1L))
  d = as.data.frame(cbind(prob, algo, repls=repls))
  diff = setdiff(show, colnames(d))
  if (length(diff) > 0)
    stopf("Trying to select columns in arg 'show' which are not available: %s", collapse(diff))
  ddply(d, show, function(x) c(.count=sum(x$repls)))
}
