reg = makeRegistry(result.dir="lido_results", wipe.result.dir=TRUE)

p1 = makeProblem("p1", 1) 
p2 = makeProblem("p2", 2) 

a1 = makeAlgorithm(id="a1", fun=function(e,p) {Sys.sleep(30);list(y=3*p)})
a2 = makeAlgorithm(id="a2", fun=function(e,p) {Sys.sleep(10);list(y=p^2)})

print(reg)
reg = addExperiments(reg, list(p1, p2), list(a1, a2), 4)

submitExps(reg, dry.run = TRUE)
z = blockIndices(1:length(reg$exps), block.size=4)
submitExps(reg, z$blocks, dry.run = TRUE)

submitExps(reg, dry.run = FALSE)
#showLogs(reg, 1)
#data = reduceResults(reg, collect=function(r) data.frame(r$y), modes=c(y="numeric"))
#print(data)
