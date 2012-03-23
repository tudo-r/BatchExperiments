library(staticdocs)
# see https://github.com/hadley/staticdocs/
ddir = "html"

if(file.exists(ddir)) 
  unlink(ddir, recursive = TRUE)
build_package("BatchExperiments", base_path = ddir)
