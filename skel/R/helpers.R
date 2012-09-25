# FIXME: move this to BBmisc
setClasses = function(x, classes) {
  class(x) = classes
  x
}

setRowNames = function(obj, nm) {
  rownames(obj) = nm
  obj
}

setColNames = function(obj, nm) {
  colnames(obj) = nm
  obj
}
