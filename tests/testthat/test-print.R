
withr::local_envvar(c("NO_COLOR" = "true"))

test_that("print() contains proper fields", {
  fields <- c('Status',
              'Absolute Model Path',
              'YAML & Model Files',
              'Tags')

  for (mod in list(NMBAYES_MOD1, STAN_MOD1)) {
    # Call capture.output() to prevent stdout stream from polluting test output.
    capture.output(
      purrr::walk(fields, ~ expect_message(print(mod), regexp = .x)))
  }
})
