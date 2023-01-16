
skip_long_tests("skipping long-running Stan gq submit_model tests")

local_model_tempdir <- function(clean = TRUE, .local_envir = parent.frame()) {
  tdir <- local_test_dir(clean = clean, .local_envir = .local_envir)
  mdir <- file.path(tdir, "model", "stan")
  fs::dir_create(mdir)
  copy_model_from(STAN_MOD3, file.path(mdir, "bern"))
  withr::local_dir(tdir, .local_envir = .local_envir)
}

local_model_tempdir()

test_that("stan gq: submit_model() works with copied model", {
  mod1 <- read_model(file.path("model", "stan", "bern"))
  # Run the Stan model to generated posterior CSVs.
  res_output_mcmc <- capture.output(submit_model(mod1, .mode = "local"))
  expect_match(res_output_mcmc, "Running MCMC", all = FALSE)

  mod2 <- copy_stan_model_as_gq(mod1, "bern-gq", .add_tags = "child") %>%
    set_stanargs(list(sig_figs = 3))

  add_stanmod_file(
    mod2,
    system.file("model", "stan", "bern-gq", "bern-gq.stan",
                package = "bbr.bayes", mustWork = TRUE))

  check_stan_model(mod2)
  res_output_gq <- capture.output(
    res_submit <- submit_model(mod2, .mode = "local"))

  expect_s3_class(res_submit, STAN_GQ_FIT_CLASS)
  expect_match(res_output_gq,
               "standalone generated quantities after 4 MCMC chains",
               all = FALSE)

  checkmate::expect_file_exists(
    build_path_from_model(mod2, STAN_MODEL_FIT_RDS))

  res_cfg <- jsonlite::fromJSON(file.path(get_output_dir(mod2),
                                          "bbi_config.json"))
  expect_true(any(stringr::str_detect(names(res_cfg), "md5$")))
})

test_that("stan gq: cmdstanr fit object can be reloaded", {
  fit <- read_fit_model(file.path("model", "stan", "bern-gq"))
  expect_s3_class(fit, STAN_GQ_FIT_CLASS)
})

test_that("stan gq: supports posterior as_draws methods", {
  mod <- read_model(file.path("model", "stan", "bern-gq"))

  draws_list <- posterior::as_draws_list(mod)
  expect_s3_class(draws_list, "draws_list")

  expect_identical(posterior::niterations(draws_list), 100L)
  expect_identical(posterior::nchains(draws_list), 4L)
})

test_that("stan gq: run_log() captures runs correctly", {
  log_df <- run_log(file.path("model", "stan"))
  expect_identical(nrow(log_df), 2L)
  expect_identical(ncol(log_df), 10L)
  expect_setequal(basename(log_df[[ABS_MOD_PATH]]),
                  c("bern", "bern-gq"))
})

test_that("stan gq: summary_log() captures runs correctly", {
  skip_if_no_bbi()

  withr::with_options(list(bbr.bbi_exe_path = bbr::read_bbi_path()), {
    log_df <- summary_log(file.path("model", "stan"))
  })
  expect_identical(nrow(log_df), 2L)
  expect_setequal(basename(log_df[[ABS_MOD_PATH]]),
                  c("bern", "bern-gq"))

  n_class <- function(cls) {
    sum(purrr::map_lgl(log_df[[SL_SUMMARY]], ~ inherits(.x, cls)))
  }

  expect_identical(n_class(STAN_FIT_CLASS), 1L)
  expect_identical(n_class(STAN_GQ_FIT_CLASS), 1L)
})