# Re-runs example models and refreshes all reference objects

if (requireNamespace("cmdstanr", quietly = TRUE)) {
  source(file.path(.proj_root, "data-raw", "run-stan-test-model.R"))
}
