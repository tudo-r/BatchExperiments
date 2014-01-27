options(BBmisc.ProgressBar.style="off")

conf = getConfig()
conf$default.resources$walltime = 120
conf$default.resources$memory = 512
conf$mail.start = conf$mail.done = conf$mail.error = "none"
setConfig(conf = conf)

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
