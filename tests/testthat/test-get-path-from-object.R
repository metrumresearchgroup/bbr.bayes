
test_that("get_model_path.bbi_stan_model() builds the right path", {
  skip_if_no_stan("get_model_path.bbi_stan_model()")
  expect_identical(get_model_path(STAN_MOD1), paste0(STAN_ABS_RUN_ROOT, STANMOD_SUFFIX))
})

test_that("get_output_dir.bbi_stan_model() builds the right path", {
  skip_if_no_stan("get_output_dir.bbi_stan_model()")
  expect_identical(get_output_dir(STAN_MOD1), paste0(STAN_ABS_RUN_ROOT, STAN_OUTDIR_SUFFIX))
})

test_that("get_yaml_path.bbi_stan_model() builds the right path", {
  skip_if_no_stan("get_yaml_path.bbi_stan_model()")
  expect_identical(get_yaml_path(STAN_MOD1), paste0(file.path(STAN_ABS_MODEL_DIR, STAN_MOD_ID), ".yaml"))
})

test_that("get_model_id parses bbi_stan_model object", {
  skip_if_no_stan("get_model_id parses bbi_stan_model object")
  expect_identical(get_model_id(STAN_MOD1), STAN_MOD_ID)
})

test_that("get_data_path parses bbi_stan_model object", {
  skip_if_no_stan("get_data_path parses bbi_stan_model object")
  expect_identical(get_data_path(STAN_MOD1), paste0(STAN_ABS_RUN_ROOT, STANDATA_JSON_SUFFIX))
})
