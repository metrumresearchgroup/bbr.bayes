
test_that("stan: config_log() works", {
  log_df <- config_log(STAN_ABS_MODEL_DIR)
  expect_equal(log_df[[RUN_ID_COL]], STAN_MOD_ID)
  expect_equal(log_df[['bbi_version']], STAN_BBI_VERSION_STRING)
  expect_false(log_df[['model_has_changed']])
  expect_false(log_df[['data_has_changed']])
})

test_that("stan: config_log() builds data", {
  perturb_file(
    system.file("extdata", "fxa.data.csv", package = "bbr.bayes"),
    txt = paste(rep(99, 8), collapse = ",")
  )
  log_df <- config_log(STAN_ABS_MODEL_DIR)
  expect_equal(log_df[[RUN_ID_COL]], STAN_MOD_ID)
  expect_equal(log_df[['bbi_version']], STAN_BBI_VERSION_STRING)
  expect_false(log_df[['model_has_changed']])
  expect_true(log_df[['data_has_changed']])
})
