
STAN_ABS_MODEL_DIR <- system.file("model", "stan",   package = "bbr.bayes")

STAN_MOD_ID <- "fxa"
STAN_MODEL_DIR <-   fs::path_rel(STAN_ABS_MODEL_DIR, getwd()) %>% as.character()
STAN_MOD1_PATH <- file.path(STAN_MODEL_DIR, STAN_MOD_ID)
STAN_MOD1 <- read_model(STAN_MOD1_PATH)
STAN_MOD_ID2 <- paste0(STAN_MOD_ID, "2")

STAN_ABS_RUN_ROOT <- file.path(STAN_ABS_MODEL_DIR, STAN_MOD_ID, STAN_MOD_ID)

STAN_SMP_DIAG_CLASS <- "draws_array"
STAN_SMP_DIAG_DIM <- c(100, 4, 6)
