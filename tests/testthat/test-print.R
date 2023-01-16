
withr::local_envvar(c("NO_COLOR" = "true"))

test_that("print() contains proper fields", {
  fields <- c('Status',
              'Absolute Model Path',
              'YAML & Model Files',
              'Tags')

  for (mod in list(NMBAYES_MOD1, STAN_MOD1, STAN_GQ_MOD)) {
    # Call capture.output() to prevent stdout stream from polluting test output.
    capture.output(
      purrr::walk(fields, ~ expect_message(print(mod), regexp = .x)))
  }
})

test_that("stan: print includes -init.R", {
  expect_output(print(STAN_MOD1), regexp = "-init.R")
})

test_that("stan gq: print includes -fitted-params.R", {
  expect_output(print(STAN_GQ_MOD), regexp = "-fitted-params.R")
})
