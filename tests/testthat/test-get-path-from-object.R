
### NONMEM Bayes

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
