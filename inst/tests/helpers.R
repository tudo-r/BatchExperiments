options(BBmisc.ProgressBar.style="off")

makeTestRegistry = function() {
  dir = "unittests-files"
  if(unlink(dir, recursive=TRUE) != 0)
    stop("Could not delete unittests files!")
  makeExperimentRegistry(
    id = "foo",
    seed = 1,
    file.dir = dir
  )
}
