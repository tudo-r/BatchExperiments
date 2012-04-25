getAlgorithmFilePath = function(file.dir, id) {
  file.path(file.dir, "algorithms", paste(id, "RData", sep="."))
}

getProblemFilePaths = function(file.dir, id) {
  parts = c("static", "dynamic")
  structure(file.path(file.dir, "problems", sprintf("%s_%s.RData", id, parts)),
            names = parts)
}
