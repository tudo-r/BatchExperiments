library(mlr)
library(mlbench)
data(Sonar)
library(BatchExperiments)

reg = makeRegistry(id="mlr", packages="mlr")
reg = makeExperimentRegistry(reg)

p1 = addProblem(reg, "iris", obj=makeClassifTask(data=iris, target="Species")) 
p2 = addProblem(reg, "sonar", obj=makeClassifTask(data=Sonar, target="Class")) 

myresample = function(obj, gen, learner, ...) {
  task = obj
  learner = makeLearner(learner, ...)
  res = makeResampleDesc("Holdout")
  err = resample(learner, task, res)$aggr["mmce.test.mean"]
  list(err = err)
}
a = addAlgorithm(reg, "resample", fun=myresample)
ad1 = makeDesign(a, exhaustive=list(learner=c("classif.lda", "classif.rpart")))
des = generateDesign(5, lower=c(sigma=0.01, C=0.01), upper=c(sigma=100, C=100))
ad2 = makeDesign(a, design = des, exhaustive=list(learner=c("classif.ksvm")))

addExperiments(reg, list(makeDesign(p1), makeDesign(p2)), list(ad1, ad2))
#submitJobs(reg)
#inds = findMissingResults(reg)
#stopifnot(length(inds) == 0)
#data = reduceResultsSimple(reg, fun=function(j,r) data.frame(err=r$err))
#print(data)
#print(ddply(data, c("prob", "algo", "learner"), function(d) c(err=mean(d$err))))

  