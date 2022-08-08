
skip_if_not_drone_or_metworx("test-model-summary")

test_that("model_summary.bbi_stan_model correctly calls read_fit_model", {
  expect_warning(
    res <- model_summary(STAN_MOD1)
  )
  expect_true(inherits(res, STAN_FIT_CLASS))

  # verify the sampler_diagnostics() method works
  smp <- res$sampler_diagnostics()
  expect_true(inherits(smp, STAN_SMP_DIAG_CLASS))
  expect_equal(dim(smp), STAN_SMP_DIAG_DIM)
})
