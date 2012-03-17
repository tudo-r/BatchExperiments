getAlgorithmFilePath = function(file.dir, id) {
  file.path(file.dir, "algorithms", paste(id, "RData", sep="."))
}

getProblemFilePath = function(file.dir, id) {
  file.path(file.dir, "problems", paste(id, "RData", sep="."))
}
