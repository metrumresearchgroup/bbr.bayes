
### NONMEM Bayes

test_that("nmbayes: read_model() works", {
  mod <- read_model(NMBAYES_MOD1_PATH)
  expect_identical(mod[[YAML_MOD_TYPE]], "nmbayes")
  expect_identical(mod[[ABS_MOD_PATH]],
                   file.path(NMBAYES_ABS_MODEL_DIR, NMBAYES_MOD_ID))
  expect_s3_class(mod, NMBAYES_MOD_CLASS)
  expect_s3_class(mod, NM_MOD_CLASS)
  expect_s3_class(mod, BBI_PARENT_CLASS)
})

test_that("nmbayes: new_model() errors without .model_type", {
  expect_error(
    new_model(file.path(NMBAYES_MODEL_DIR, "test-nmbayes")),
    regexp = "IF THIS IS NOT A NONMEM MODEL"
  )
})

test_that("nmbayes: new_model() works", {
  tdir <- local_test_dir()
  model_path <- file.path(tdir, "100")
  fs::file_copy(paste0(NMBAYES_MOD1_PATH, ".ctl"),
                paste0(model_path, ".ctl"))
  mod <- new_model(model_path, .model_type = "nmbayes")

  expect_equal(mod[[YAML_MOD_TYPE]], "nmbayes")
  expect_equal(mod[[ABS_MOD_PATH]], model_path)
  expect_s3_class(mod, NMBAYES_MOD_CLASS)
  expect_s3_class(mod, NM_MOD_CLASS)
  expect_s3_class(mod, BBI_PARENT_CLASS)
})

### Stan

test_that("stan: read_model() works", {
  .m <- read_model(STAN_MOD1_PATH)
  expect_equal(.m[[YAML_MOD_TYPE]], "stan")
  expect_equal(.m[[ABS_MOD_PATH]], file.path(STAN_ABS_MODEL_DIR, STAN_MOD_ID))
  expect_s3_class(.m, STAN_MOD_CLASS)
  expect_s3_class(.m, BBI_PARENT_CLASS)
})

test_that("stan: new_model() errors without .model_type", {
  expect_error(
    new_model(file.path(STAN_MODEL_DIR, "testmod_new_model1")),
    regexp = "IF THIS IS NOT A NONMEM MODEL"
  )
})

test_that("stan: new_model() works", {
  mod_name <- "testmod_new_model2"
  expect_message(
    .m <- new_model(file.path(STAN_MODEL_DIR, mod_name), .model_type = "stan"),
    regexp = sprintf(MISSING_STAN_FILES_ERR_MSG, "bbi_stan_model")
  )
  on.exit(cleanup_model(.m))

  expect_equal(.m[[YAML_MOD_TYPE]], "stan")
  expect_equal(.m[[ABS_MOD_PATH]], file.path(STAN_ABS_MODEL_DIR, mod_name))
  expect_s3_class(.m, STAN_MOD_CLASS)
  expect_s3_class(.m, BBI_PARENT_CLASS)
})

test_that("stan_gq: new_model() works", {
  tdir <- local_test_dir()
  expect_message(
    m <- new_model(file.path(tdir, "gq"), .model_type = "stan_gq"),
    regexp = sprintf(MISSING_STAN_FILES_ERR_MSG, "bbi_stan_gq_model"))

  expect_identical(m[[YAML_MOD_TYPE]], "stan_gq")
  expect_identical(m[[ABS_MOD_PATH]], file.path(tdir, "gq"))
  expect_s3_class(m, STAN_GQ_MOD_CLASS)
  expect_s3_class(m, STAN_MOD_CLASS)
  expect_s3_class(m, BBI_PARENT_CLASS)
})
