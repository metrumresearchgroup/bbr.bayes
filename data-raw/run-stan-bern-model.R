devtools::load_all()

run_stan_bern_test_model <- function() {
  bern_dir <- file.path("inst", "model", "stan", "bern")
  if (!dir.exists(bern_dir)) {
    stop("script must be invoked from top of bbr.bayes directory")
  }

  mod <- read_model(bern_dir)
  message("[", Sys.time(), "] Starting bern run")
  submit_model(mod, .mode = "local", .overwrite = TRUE)
  message("[", Sys.time(), "] Finished bern run")

  mod_gq <- read_model(file.path("inst", "model", "stan", "bern-gq"))
  message("[", Sys.time(), "] Starting bern-gq model run")
  submit_model(mod_gq, .mode = "local", .overwrite = TRUE)
  message("[", Sys.time(), "] Finished bern-gq model run")
}

run_stan_bern_test_model()
