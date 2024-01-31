
test_that("model_summary() errors and points to read_fit_model()", {
  for (mod in list(NMBAYES_MOD1, STAN_MOD1, STAN_GQ_MOD)) {
    expect_error(model_summary(mod), "read_fit_model")
  }
})
