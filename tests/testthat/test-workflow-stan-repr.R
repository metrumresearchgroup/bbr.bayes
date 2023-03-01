
skip_long_tests("skipping long-running Stan reproducibility tests")

local_stan_bern_model()

submit <- function(mod) {
  capture.output(submit_model(mod, .mode = "local", .overwrite = TRUE))
  posterior::extract_variable(posterior::as_draws(mod), "theta")
}

expect_same_result_across_runs <- function(mod) {
  theta1 <- submit(mod)
  theta2 <- submit(mod)
  expect_equal(theta1, theta2)
}

write_init_body <- function(mod, body_lines) {
  initfile <- build_path_from_model(mod, STANINIT_SUFFIX)
  lines <- c("make_init <- function(.data, .args) {",
             body_lines,
             "}")
  writeLines(lines, initfile)
}

test_that("stan: same results on rerun: init = NULL", {
  mod <- read_model(file.path("model", "stan", "bern"))
  expect_same_result_across_runs(mod)
})

test_that("stan: same results on rerun: init = fixed scalar", {
  mod <- read_model(file.path("model", "stan", "bern"))
  write_init_body(mod, "1")
  expect_same_result_across_runs(mod)
})

test_that("stan: same results on rerun: init = fixed list", {
  mod <- read_model(file.path("model", "stan", "bern"))
  write_init_body(
    mod,
    deparse(purrr::map(seq(0.2, 0.8, by = 0.2),
                       ~ list(theta = .x))))
  expect_same_result_across_runs(mod)
})
