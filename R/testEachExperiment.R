#' Test one job of each experiment by running it with Rscript in a new process.
#'
#' @description
#' Test (sequentially) one job of 'each experiment', where 'each experiment' corresponds to
#' one row of \code{\link{summarizeExperiments}}(Registry).
#' One job of each experiment is randomly chosen, if .count > 1,
#' from amongst the job ids corresponding to that row of
#' \code{summarizeExperiments}(Registry).  This is particularly useful for debugging.
#' One can exclude certain prob.patterns or algo.patterns.
#' Note that neither the registry, database or file directory are changed.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param excld.prob.pattern [\code{character(1)}]\cr
#'   If not missing, all problem ids that match this string are excluded.
#' @param excld.algo.pattern [\code{character(1)}]\cr
#'   If not missing, all algorithm ids that match this string are excluded.
#' @param ... further arguments passed to \code{\link{testJob}}.
#' @return [list]. A list of (potentially) 2-lists. [list][[1]] contains the
#' results of \code{\link{getJob}}. If \code{\link{testJob}}
#' was successful, then [list][[2]] contains the results.
#' Otherwise, [list][[2]] does not exist.
#' @family debug
#' @export
#' @author Edward A. Roualdes
#' @examples
#' reg = makeExperimentRegistry(id = "example1", file.dir = tempfile())
#' p1 = addProblem(reg, "one", 1)
#' p2 = addProblem(reg, "two", 2)
#' a = addAlgorithm(reg, "A", fun = function(static, n) static + n)
#' b = addAlgorithm(reg, "B", fun = function(static, n) stop("Ahh!"))
#' addExperiments(reg, algo.design = makeDesign(a, exhaustive = list(n = 1:4)))
#' addExperiments(reg, algo.design = makeDesign(b, exhaustive = list(n = 1:4)))
#' summarizeExperiments(reg)
#' res = testEachExperiment(reg, external=FALSE)
#' which(sapply(res, length)==1)           # indices of res that failed

testEachExperiment = function(reg, excld.prob.pattern=NULL, excld.algo.pattern=NULL, ...) {

    ## exclude specified rows
    sexps = summarizeExperiments(reg)
    not_prbs = setdiff(sexps$prob, excld.prob.pattern)
    not_algs = setdiff(sexps$algo, excld.algo.pattern)
    sub_sexps = subset(sexps, prob %in% not_prbs & algo %in% not_algs)

    ## test subset of experiments
    num_unique_exps = nrow(sub_sexps)
    test_results = vector('list', num_unique_exps)
    for (s in seq_len(num_unique_exps)) {
        test_results[[s]] = vector('list', 2)
        jid = findExperiments(reg, prob.pattern=sub_sexps$prob[s],
            algo.pattern=sub_sexps$algo[s], match.substring=FALSE)
        idx = ifelse(length(jid)>1, sample(jid, 1), jid)
        test_results[[s]][[1]] = getJob(reg, id=idx)
        ## overly complex handling of res based on external = T/F
        res = NULL
        res = tryCatch(BatchJobs::testJob(reg, id=idx, ...),
            error = function(e) NULL)
        test_results[[s]][[2]] = res
    }
    return(test_results)
}
