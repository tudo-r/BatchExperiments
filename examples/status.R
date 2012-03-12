reg = makeRegistry(result.dir="status_results", wipe.result.dir=TRUE)

p1 = makeProblem("p1", obj=1) 
p2 = makeProblem("p2", obj=2) 

a1 = makeAlgorithm(id="a1", fun=function(e, p) {Sys.sleep(1); list(y=999)})
a2 = makeAlgorithm(id="a2", fun=function(e, p) stop(123))

print(reg)
reg = addExperiments(reg, list(p1, p2), list(a1, a2), 3)
lapply(reg$exps, doExperiment)
message("\n")

inds =  findMissingResults(reg, print=TRUE)
stopifnot(length(inds) == length(reg$exps)/2)
print(showStatus(reg, details=FALSE))
print(showStatus(reg, details=TRUE))
print(showStatus(reg, details=TRUE, show.times=TRUE))
