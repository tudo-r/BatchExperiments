library(BatchExperiments)
library(soobench)

reg = makeRegistry(file.dir = "examples_files", packages="soobench")
reg = makeExperimentRegistry(reg)

p1 = addProblem(reg, "branin", gen=function(obj, dim) branin_function()) 
p2 = addProblem(reg, "rastrigin", gen=function(obj, dim) rastrigin_function(dim)) 
p3 = addProblem(reg, "rosenbrock", gen=function(obj, dim) rosenbrock_function(dim)) 

myoptim = function(obj, gen, method, maxit) {
  # set random start value
  control = list(maxit=10)
  par = rnorm(number_of_parameters(gen))
  optim(par, gen, control=control)
}

a = addAlgorithm(reg, id="optim", fun=myoptim)

pd1 = makeDesign(p1, exhaustive=list(dim=2))
pd2 = makeDesign(p2, exhaustive=list(dim=2:3))
pd3 = makeDesign(p3, exhaustive=list(dim=2:3))

ad  = makeDesign(a, exhaustive=list(method=c("Nelder-Mead", "BFGS")))

addExperiments(reg, list(pd1, pd2, pd3), ad)

#submitJobs(reg)
#inds = findMissingResults(reg)
#stopifnot(length(inds) == 0)
#data = reduceResults(reg, collect=function(x) data.frame(x$val), modes=c(val="numeric"))
#print(data)
#print(ddply(data, c("prob", "algo", "dimensions"), function(d) c(opt=mean(d$val))))
