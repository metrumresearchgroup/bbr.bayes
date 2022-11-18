devtools::load_all()

run_nmbayes_test_model <- function() {
  if (!file.exists(file.path("inst", "extdata", "analysis3.csv"))) {
    stop("script must be invoked from top of bbr.bayes directory")
  }

  model_dir <- file.path("inst", "model", "nonmem", "bayes")
  bbr::bbi_init(model_dir, "/opt/NONMEM", "nm75")
  mod <- read_model(file.path(model_dir, "1100"))
  message("[", Sys.time(), "] Starting model run")
  submit_model(mod, .mode = "local", .overwrite = TRUE)
  message("[", Sys.time(), "] Finished model run")
}

run_nmbayes_test_model()
