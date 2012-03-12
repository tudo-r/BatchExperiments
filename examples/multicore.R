if (require("multicore")) {
  # set result.dir to NULL so no results are written
  reg = makeRegistry(result.dir=NULL)

  p1 = makeProblem("p1", obj=1) 
  p2 = makeProblem("p2", obj=2) 

  # do something time-consuming so we see the processes....
  a1 = makeAlgorithm(reg, id="a1", fun=function(e, p) {x=rnorm(1000);list(val=3*p)})
  a2 = makeAlgorithm(reg, id="a2", fun=function(e, p) {x=rnorm(1000);list(val=p^2)})

  print(reg)
  exps = addExperiments(reg, list(p1,p2), list(a1, a2))
  results = mclapply(reg$exps, doExperiment, mc.cores=2)
  data = reduceResults(reg, results=results, 
    collect=function(x) data.frame(x$val), modes=c(val="numeric"))
  print(data)
}