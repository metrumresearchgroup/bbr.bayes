
skip_if_not_drone_or_metworx("test-print")

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {
  withr::local_envvar(c("NO_COLOR" = "true"))

  test_that("print.bbi_stan_model contains proper fields", {
    fields <- c('Status',
                'Absolute Model Path',
                'YAML & Model Files',
                'Tags')

    bullets <- capture.output({ # these get thrown away, but we don't want them to print in the test output
      purrr::walk(fields, ~ expect_message(print(STAN_MOD1), regexp = .x))
    })
  })
})
