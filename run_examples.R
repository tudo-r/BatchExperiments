if(!exists("use.package")) {
  use.package = !interactive()  
}
  
cf = "clusterFunctions.R"

if (use.package) {
  message("Using installed copy of package for tests!")
  require("BatchExperiments")  
} else {
  source("test_init_interactive.R")
  setBatchJobsConf(cluster.functions=cf)  
  sourceClusterFunctions(EE_CONF$cluster.functions)
} 

source("examples/status.R")
source("examples/mlr_registry.R")
source("examples/soo_registry.R")
source("examples/multicore.R")
#source("examples/lido.R")
