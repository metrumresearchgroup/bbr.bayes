
### NONMEM Bayes

test_that("nmbayes: build_path_from_model points caller to chain_paths", {
  rlang::reset_message_verbosity("bbr.bayes/build_path_from_model->chain_paths")
  expect_message(
    res <- build_path_from_model(NMBAYES_MOD1, ".foo"),
    "chain_paths"
  )
  expect_identical(
    res,
    file.path(NMBAYES_MOD1[[ABS_MOD_PATH]], paste0(NMBAYES_MOD_ID, ".foo"))
  )
})

test_that("nmbayes: get_model_path() builds the right path", {
  expect_identical(get_model_path(NMBAYES_MOD1),
                   paste0(file.path(NMBAYES_ABS_MODEL_DIR, NMBAYES_MOD_ID),
                          ".ctl"))
})

test_that("nmbayes: get_output_dir() builds the right path", {
  expect_identical(get_output_dir(NMBAYES_MOD1),
                   file.path(NMBAYES_ABS_MODEL_DIR, NMBAYES_MOD_ID))
})

test_that("nmbayes: get_yaml_path() builds the right path", {
  expect_identical(get_yaml_path(NMBAYES_MOD1),
                   paste0(file.path(NMBAYES_ABS_MODEL_DIR, NMBAYES_MOD_ID),
                          ".yaml"))
})

test_that("nmbayes: get_model_id() parses model object", {
  expect_identical(get_model_id(NMBAYES_MOD1),
                   NMBAYES_MOD_ID)
})

test_that("nmbayes: get_data_path() parses model object", {
  expect_identical(get_data_path(NMBAYES_MOD1),
                   system.file("extdata", "analysis3.csv",
                               package = "bbr.bayes"))
})

test_that("nmbayes: get_data_path() supports .check_exists", {
  tdir <- local_test_dir()

  model_dir <- file.path(tdir, "model", "nonmem", "bayes")
  fs::dir_create(model_dir)
  fs::dir_copy(
    system.file(
      "model", "nonmem", "bayes", "1100",
      package = "bbr.bayes", mustWork = TRUE
    ),
    model_dir
  )
  fs::file_copy(
    system.file(
      "model", "nonmem", "bayes", c("1100.ctl", "1100.yaml"),
      package = "bbr.bayes", mustWork = TRUE
    ),
    model_dir
  )

  mod <- read_model(file.path(model_dir, "1100"))
  expect_error(get_data_path(mod), "does not exist")

  data_dir <- file.path(tdir, "extdata")
  data_path <- file.path(data_dir, "analysis3.csv")
  expect_identical(get_data_path(mod, .check_exists = FALSE), data_path)

  fs::dir_create(data_dir)
  cat("", file = data_path)
  expect_identical(get_data_path(mod), data_path)
})

test_that("nmbayes: get_data_path() falls back to control stream path", {
  tdir <- local_test_dir()
  model_dir <- file.path(tdir, "model", "nonmem", "bayes")
  fs::dir_create(model_dir)

  mod <- copy_model_from(NMBAYES_MOD1, file.path(model_dir, get_model_id(NMBAYES_MOD1)))
  data_path <- file.path(tdir, "extdata", "analysis3.csv")

  expect_error(get_data_path(mod), "does not exist")
  expect_identical(get_data_path(mod, .check_exists = FALSE), data_path)

  fs::dir_create(file.path(tdir, "extdata"))
  cat("", file = data_path)
  expect_identical(get_data_path(mod), data_path)
})

### Stan

test_that("stan: get_model_path() builds the right path", {
  expect_identical(get_model_path(STAN_MOD1), paste0(STAN_ABS_RUN_ROOT, STANMOD_SUFFIX))
})

test_that("stan: get_output_dir() builds the right path", {
  expect_identical(get_output_dir(STAN_MOD1), paste0(STAN_ABS_RUN_ROOT, STAN_OUTDIR_SUFFIX))
})

test_that("stan: get_yaml_path() builds the right path", {
  expect_identical(get_yaml_path(STAN_MOD1), paste0(file.path(STAN_ABS_MODEL_DIR, STAN_MOD_ID), ".yaml"))
})

test_that("stan: get_model_id() parses model object", {
  expect_identical(get_model_id(STAN_MOD1), STAN_MOD_ID)
})

test_that("stan: get_data_path() parses model object", {
  expect_identical(get_data_path(STAN_MOD1), paste0(STAN_ABS_RUN_ROOT, STANDATA_JSON_SUFFIX))
})
