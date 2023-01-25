
test_that("run_log() works", {
  log_df <- run_log(system.file("model", package = "bbr.bayes"),
                    .recurse = TRUE)
  expect_setequal(!!unique(log_df[[YAML_MOD_TYPE]]),
                  c("nmbayes", "nonmem", "stan", "stan_gq"))

  nmbayes_df <- dplyr::filter(log_df, model_type == "nmbayes")
  expect_identical(nrow(nmbayes_df), 1L)
  expect_identical(nmbayes_df[[RUN_ID_COL]], NMBAYES_MOD_ID)

  stan_df <- dplyr::filter(log_df, model_type == "stan")
  expect_equal(nrow(stan_df), 2L)
  expect_identical(stan_df[[RUN_ID_COL]],
                   c(STAN_MOD_ID3, STAN_MOD_ID))
})
