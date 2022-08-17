
test_that("read_model() works for Stan model", {
  .m <- read_model(STAN_MOD1_PATH)
  expect_equal(.m[[YAML_MOD_TYPE]], "stan")
  expect_equal(.m[[ABS_MOD_PATH]], file.path(STAN_ABS_MODEL_DIR, STAN_MOD_ID))
  expect_s3_class(.m, STAN_MOD_CLASS)
  expect_s3_class(.m, BBI_PARENT_CLASS)
})

test_that("new_model() errors for Stan model without .model_type", {
  expect_error(
    new_model(file.path(STAN_MODEL_DIR, "testmod_new_model1")),
    regexp = "IF THIS IS NOT A NONMEM MODEL"
  )
})

test_that("new_model() works for Stan model", {
  mod_name <- "testmod_new_model2"
  expect_message(
    .m <- new_model(file.path(STAN_MODEL_DIR, mod_name), .model_type = "stan"),
    regexp = MISSING_STAN_FILES_ERR_MSG
  )
  on.exit(cleanup_model(.m))

  expect_equal(.m[[YAML_MOD_TYPE]], "stan")
  expect_equal(.m[[ABS_MOD_PATH]], file.path(STAN_ABS_MODEL_DIR, mod_name))
  expect_s3_class(.m, STAN_MOD_CLASS)
  expect_s3_class(.m, BBI_PARENT_CLASS)
})
