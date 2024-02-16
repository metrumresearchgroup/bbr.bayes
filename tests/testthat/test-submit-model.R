test_that("stan: submit_model aborts if .bbi_args is passed", {
  for (m in list(STAN_MOD1, STAN_GQ_MOD)) {
    expect_error(submit_model(m, .bbi_args = "foo"), ".bbi_args", fixed = TRUE)
  }
})
