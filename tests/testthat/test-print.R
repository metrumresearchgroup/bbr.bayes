
withr::local_envvar(c("NO_COLOR" = "true"))

test_that("print.bbi_stan_model contains proper fields", {
  fields <- c('Status',
              'Absolute Model Path',
              'YAML & Model Files',
              'Tags')

  # Call capture.output() to prevent stdout stream from polluting test output.
  capture.output(
    purrr::walk(fields, ~ expect_message(print(STAN_MOD1), regexp = .x)))
})
