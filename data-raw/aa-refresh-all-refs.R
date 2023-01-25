# Re-runs example models and refreshes all reference objects

source(file.path(.proj_root, "data-raw", "run-stan-fxa-model.R"))
source(file.path(.proj_root, "data-raw", "run-stan-bern-model.R"))
source(file.path(.proj_root, "data-raw", "run-nmbayes-test-model.R"))
