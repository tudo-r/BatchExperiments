# Generates crossproduct of exhaustive options joined with rows of a given design data.frame.
# State-based object with iterator functions is returned.
# If both arguments are empty, the a design is created with corresponds to one 'point' that
# defines a function call with no arguments.
# @param ex [\code{list}]\cr
#   Named list of parameters settings which should be exhaustively tried.
#   All elements of the list must be primitive vectors like numeric, integer, factor, etc.
# @param .design [\code{data.frame}]\cr
#   The design. Rows define one 'point'.
# @return List of funs. nextElem returns a named list.
designIterator = function(ex, .design = data.frame()) {
  nextState = function(state, pos = 1L) {
    if (state[pos] < state.last[pos])
      replace(state, pos, state[pos] + 1L)
    else
      nextState(replace(state, pos, 1L), pos + 1L)
  }

  nextElem = function() {
    state <<- nextState(state)
    counter <<- counter + 1L

    c(as.list(.design[state[! is.ex.state], , drop = FALSE]),
      mapply(function(n, s) ex[[n]][s],
             n = names.ex.state,
             s = state[is.ex.state],
             SIMPLIFY = FALSE))
  }

  hasNext = function() {
    counter < counter.max
  }

  reset = function() {
    state <<- state.init
    counter <<- 0L
    invisible(TRUE)
  }

  state.last = sort(setNames(c(vapply(ex, length, 1L), max(nrow(.design), 1L)), c(names(ex), ".design.row")), decreasing = TRUE)
  state.init = setNames(c(0L, rep(1L, length(state.last) - 1L)), names(state.last))
  counter.max = prod(state.last)
  if (counter.max > .Machine$integer.max)
    stop("The generated design is too big. Designs with up to ",
         .Machine$integer.max, " rows are supported!")
  counter.max = as.integer(counter.max)
  is.ex.state = (names(state.init) != ".design.row")
  names.ex.state = names(state.init)[is.ex.state]

  state = state.init
  counter = 0L

  list(nextElem = nextElem,
       hasNext = hasNext,
       reset = reset,
       n.states = counter.max)
}

#' Create parameter designs for problems and algorithms.
#'
#' Create a parameter design for either a problem or an algorithm that you
#' can use in \code{\link{addExperiments}}.
#' All parameters in \code{design} and \code{exhaustive} be \dQuote{primitive}
#' in the sense that either \code{is.atomic} is \code{TRUE} or \code{is.factor} is \code{TRUE}.
#'
#' Be aware of R's default behaviour of converting strings into factors if you use the \code{design}
#' parameter. See option \code{stringsAsFactors} in \code{\link{data.frame}} to turn this off.
#' @param id [\code{character(1)}]\cr
#'   Id of algorithm or problem.
#' @param design [\code{data.frame}]\cr
#'   The design. Must have named columns corresponding to parameters.
#'   Default is an empty \code{data.frame()}.
#' @param exhaustive [\code{list}]\cr
#'   Named list of parameters settings which should be exhaustively tried.
#'   Names must correspond to parameters.
#'   Default is empty list.
#' @return [\code{\link{Design}}].
#' @export
#' @aliases Design
#' @examples
#' FIXME lapply example
#' would be nice to associate one design with multiple probs or algos with one call. makeDesigns?
makeDesign = function(id, design=data.frame(), exhaustive=list()) {
  checkArg(id, "character", len=1L, na.ok=FALSE)
  checkArg(design, "data.frame")
  checkArg(exhaustive, "list")
  if (!isProperlyNamed(exhaustive))
    stop("Argument exhaustive must be a properly named list!")
  if (!all(vapply(exhaustive, is.atomic, logical(1L))))
    stop("All elements of exhaustive must be an atomic vector type!")
  if (! all(vapply(exhaustive, length, integer(1L)) >= 1L))
    stop("All elements of exhaustive must have at least have length 1!")
  if (anyDuplicated(c(names(design), names(exhaustive))) > 0L)
    stop("Duplicated design parameters found!")

  if (ncol(design) > 0L) {
    if (!all(vapply(design, function(x) is.atomic(x) | is.factor(x), logical(1L))))
      stop("All columns of design must be either of atomic type or a factor!")
  }
  setClasses(list(id = id, designIter=designIterator(exhaustive, .design = design)),
             "Design")
}

#' @S3method print Design
print.Design = function(x, ...) {
  n = x$designIter$n.states
  catf("Design for %s with %i row%s", x$id, n, ifelse(n == 1L, "", "s"))
}
