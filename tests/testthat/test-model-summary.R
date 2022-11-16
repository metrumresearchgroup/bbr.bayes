
test_that("nmbayes: model_summary() summarizes submodels", {
  withr::with_options(list(bbr.bbi_exe_path = bbr::read_bbi_path()), {
    expect_warning(res <- model_summary(NMBAYES_MOD1),
                   "not implemented")
  })
  expect_length(res, 2)
  for (obj in res) {
    expect_s3_class(obj, "bbi_nonmem_summary")
  }
})

test_that("stan: model_summary() correctly calls read_fit_model()", {
  expect_warning(
    res <- model_summary(STAN_MOD1)
  )
  expect_true(inherits(res, STAN_FIT_CLASS))

  # verify the sampler_diagnostics() method works
  smp <- res$sampler_diagnostics()
  expect_true(inherits(smp, STAN_SMP_DIAG_CLASS))
  expect_equal(dim(smp), STAN_SMP_DIAG_DIM)
})
