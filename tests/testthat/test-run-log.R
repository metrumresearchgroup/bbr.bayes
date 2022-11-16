
test_that("run_log() works with Stan model", {
  log_df <- run_log(system.file("model", package = "bbr.bayes"))
  expect_setequal(!!unique(log_df[[YAML_MOD_TYPE]]),
                  c("nonmem", "stan"))

  stan_df <- dplyr::filter(log_df, model_type == "stan")
  expect_equal(nrow(stan_df), 1L)
  expect_equal(stan_df[[RUN_ID_COL]], STAN_MOD_ID)
})
