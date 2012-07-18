getAlgorithmFilePath = function(file.dir, id) {
  file.path(file.dir, "algorithms", sprintf("%s.RData", id))
}

getProblemFilePaths = function(file.dir, id) {
  parts = c("static", "dynamic")
  structure(file.path(file.dir, "problems", sprintf("%s_%s.RData", id, parts)),
            names = parts)
}

getStashDir = function(file.dir) {
  file.path(file.dir, "stash")
}

getStashFilePath = function(file.dir, id) {
  file.path(file.dir, "stash", sprintf("%s.RData", id))
}
