#' Add an R object to the stash.
#'
#' You can store objects in the special folder \code{stash} inside your
#' \code{file.dir}. Both problems and algorithms accept the formal argument
#' \code{stash} which makes the contents available on the nodes.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param id [\code{character(1)}]\cr
#'   Name for the stashed object.
#' @param item [\code{any}]\cr
#'   Item to stash. Can be any R object.
#' @param overwrite [\code{logical(1)}]\cr
#'   Overwrite the stashed object if it already exists?
#'   Default is \code{FALSE}.
#' @return [\code{character(1)}]. Invisibly returns the id on success.
#' @aliases Stash
#' @export
putInStash = function(reg, id, item, overwrite=FALSE) {
  checkArg(reg, cl="ExperimentRegistry")
  putStash(reg$file.dir, id, item, overwrite)
}

# internal use only
putStash = function(file.dir, id, item, overwrite=FALSE) {
  checkArg(id, cl = "character", len=1L, na.ok=FALSE)
  checkArg(overwrite, "logical", len=1L, na.ok=FALSE)
  BatchJobs:::checkIdValid(id)
  fn = getStashFilename(file.dir, id)
  if (!overwrite && file.exists(fn))
    stopf("Item with id='%s' already exists in stash", id)

  save(id, item, file=fn)
  invisible(id)
}

#' Retrieve R object from the stash.
#'
#' The previosly stashed object will be loaded and returned.
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param id [\code{character}]\cr
#'   Name of stashed object.
#' @return [\code{list}]. Named list of loaded stashed objects.
#' @export
getFromStash = function(reg, id) {
  checkArg(reg, cl="ExperimentRegistry")
  getStash(reg$file.dir, id)
}

# internal use only
getStash = function(file.dir, id) {
  checkArg(id, cl = "character", len=1L, na.ok=FALSE)
  fn = getStashFilename(file.dir, id)
  if (!file.exists(fn))
    stopf("Stashed item with id='%s' not found", id)
  load2(fn, "item")
}

#' Look up what is in the stash.
#'
#' @param reg [\code{\link{ExperimentRegistry}}]\cr
#'   Registry.
#' @param pattern [\code{character(1)}]\cr
#'   Default is \code{"*"}.
#'   Pattern to restrict stash IDs to. Will be passed to \code{\link{list.files}}.
#' @param ignore.case [\code{logical(1)}]\cr
#'   Will be passed to \code{\link{list.files}}.
#'   Default is \code{FALSE}.
#' @param details [\code{logical(1)}]\cr
#'   If set to \code{TRUE}, the output of \code{\link{str}} for each stashed object will
#'   get printed.
#'   Default is \code{FALSE}.
#' @return [\code{character}]. Vector of matched stash IDs.
#' @export
showStash = function(reg, pattern="*", ignore.case=FALSE, details=FALSE) {
  checkArg(reg, cl="ExperimentRegistry")
  listStash(reg$file.dir, pattern, ignore.case, details)
}

listStash = function(file.dir, pattern="*", ignore.case=FALSE, details=FALSE) {
  checkArg(pattern, "character", len=1L, na.ok=FALSE)
  checkArg(ignore.case, "logical", len=1L, na.ok=FALSE)
  checkArg(details, "logical", len=1L, na.ok=FALSE)
  fns = list.files(getStashPath(file.dir), pattern=pattern,
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

# stash object used on the notes
# FIXME export this, this is useful for debugging
# FIXME maybe rename
getStashObject = function(reg) {
  file.dir = reg$file.dir
  list(get = function(id)
         getStash(file.dir, id),
       put = function(id, item, overwrite=FALSE)
         putStash(file.dir, id, item, overwrite),
       list = function(pattern="*", ignore.case=FALSE)
         listStash(file.dir, pattern, ignore.case, details=FALSE))
}
