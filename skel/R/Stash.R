#' Add an R object to the stash.
#'
#' You can store objects in the special folder \code{stash} inside your
#' \code{file.dir}. Both problems and algorithms accept the parameter
#' \code{stash} as vector of IDs of stashed objects.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param id [\code{character(1)}]\cr
#'   Name of the stashed object
#' @param item [\code{any}]\cr
#'   Item to stash. Can be any R object.
#' @param overwrite [\code{logical(1)}]\cr
#'   Overwrite the stashed object if it already exists?
#'   Default is \code{FALSE}.
#' @return [\code{character(1)}]. Invisibly returns the id on success.
#' @aliases Stash
#' @export
saveToStash = function(reg, id, item, overwrite=FALSE) {
  checkArg(reg, cl="ExperimentRegistry")
  checkArg(id, cl = "character", len=1L, na.ok=FALSE)
  checkArg(overwrite, "logical", len=1L, na.ok=FALSE)
  BatchJobs:::checkIdValid(id)

  fn = getStashFilePath(reg$file.dir, id)
  if (!overwrite && file.exists(fn))
    stopf("Item with id='%s' already exists in stash", id)

  save(id, item, file=fn)
  invisible(id)
}

#' Retrieve R objects from the stash.
#'
#' The previosly stashed objects will be loaded and returned as a
#' list with stash IDs as names.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param ids [\code{character}]\cr
#'   Vector of stash IDs.
#' @return [\code{list}]. Named list of loaded stashed objects.
#' @export
getFromStash = function(reg, ids) {
  checkArg(reg, cl="ExperimentRegistry")
  checkArg(ids, cl = "character", min.len=1L, na.ok=FALSE)

  sapply(ids, getStashed, file.dir=reg$file.dir, simplify=FALSE)
}

#' Look up what is in the stash.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param pattern [\code{character(1)}]\cr
#'   Pattern to restrict stash IDs to. Will be passed to \code{\link{list.files}}.
#'   Default is \code{"*"}.
#' @param ignore.case [\code{logical(1)}]\cr
#'   Will be passed to \code{\link{list.files}}.
#'   Default is \code{FALSE}.
#' @param details [\code{logical(1)}]\cr
#'   If set to \code{TRUE}, the output of \code{\link{str}} for each stashed object will
#'   get printed.
#'   Default is \code{FALSE}.
#' @return [\code{character}]. Vector of matched stash IDs.
#' @export
showStashed = function(reg, pattern="*", ignore.case=FALSE, details=FALSE) {
  checkArg(reg, cl="ExperimentRegistry")
  checkArg(pattern, "character", len=1L, na.ok=FALSE)
  checkArg(ignore.case, "logical", len=1L, na.ok=FALSE)
  checkArg(details, "logical", len=1L, na.ok=FALSE)

  fns = list.files(getStashDir(reg$file.dir), pattern=pattern,
                   ignore.case=ignore.case, full.names=TRUE)
  ids = sub("\\.RData$", "", basename(fns))
  if (!details)
    return(ids)

  n = length(ids)
  for(i in seq_along(ids)) {
    messagef("Structure for item '%s':", ids[i])
    str(load2(fns[i], "item"))
    if(i < n) message("")
  }
  invisible(ids)
}

# internal use only
getStashed = function(file.dir, id) {
  fn = getStashFilePath(file.dir, id)
  if (!file.exists(fn))
    stopf("Stashed item with id='%s' not found", id)
  load2(fn, "item")
}
