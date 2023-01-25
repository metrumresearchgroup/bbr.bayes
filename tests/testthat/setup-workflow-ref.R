
### NONMEM Bayes

NMBAYES_ABS_MODEL_DIR <- system.file("model", "nonmem", "bayes",
                                     package = "bbr.bayes", mustWork = TRUE)

NMBAYES_MOD_ID <- "1100"
NMBAYES_MODEL_DIR <- as.character(fs::path_rel(NMBAYES_ABS_MODEL_DIR))
NMBAYES_MOD1_PATH <- file.path(NMBAYES_MODEL_DIR, NMBAYES_MOD_ID)
NMBAYES_MOD1 <- read_model(NMBAYES_MOD1_PATH)

### Stan

STAN_ABS_MODEL_DIR <- system.file("model", "stan",   package = "bbr.bayes")

STAN_MOD_ID <- "fxa"
STAN_MODEL_DIR <-   fs::path_rel(STAN_ABS_MODEL_DIR, getwd()) %>% as.character()
STAN_MOD1_PATH <- file.path(STAN_MODEL_DIR, STAN_MOD_ID)
STAN_MOD1 <- read_model(STAN_MOD1_PATH)
STAN_MOD_ID2 <- paste0(STAN_MOD_ID, "2")

STAN_MOD_ID3 <- "bern"
STAN_MOD3_PATH <- file.path(STAN_MODEL_DIR, STAN_MOD_ID3)
STAN_MOD3 <- read_model(STAN_MOD3_PATH)

STAN_GQ_MOD_ID <- "bern-gq"
STAN_GQ_MOD_PATH <- file.path(STAN_MODEL_DIR, STAN_GQ_MOD_ID)
STAN_GQ_MOD <- read_model(STAN_GQ_MOD_PATH)

STAN_ABS_RUN_ROOT <- file.path(STAN_ABS_MODEL_DIR, STAN_MOD_ID, STAN_MOD_ID)

STAN_SMP_DIAG_CLASS <- "draws_array"
STAN_SMP_DIAG_DIM <- c(100, 4, 6)
