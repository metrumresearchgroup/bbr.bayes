
skip_long_tests("skipping long-running summary_log() tests")
skip_if_no_cmdstan()

local_model_tempdir <- function(clean = TRUE, .local_envir = parent.frame()) {
  tdir <- local_test_dir(clean = clean, .local_envir = .local_envir)
  fs::dir_copy(system.file("model", package = "bbr.bayes", mustWork = TRUE),
               tdir)

  m_bern <- read_model(file.path(tdir, "model", "stan", "bern"))
  capture.output(submit_model(m_bern, .overwrite = TRUE))

  m_bern_gq <- read_model(file.path(tdir, "model", "stan", "bern-gq"))
  capture.output(submit_model(m_bern_gq, .overwrite = TRUE))

  withr::local_dir(tdir, .local_envir = .local_envir)
}

local_model_tempdir()

test_that("stan_summary_log() works", {
  slog <- stan_summary_log("model", .recurse = TRUE)

  expect_identical(nrow(slog), 3L)
  expect_setequal(slog$method, c("sample", "generate_quantities"))

  expect_identical(unique(slog$nchains), 4L)
  checkmate::expect_integer(slog$iter_warmup, all.missing = FALSE)
  checkmate::expect_integer(slog$iter_sampling, all.missing = FALSE)
  checkmate::expect_integer(slog$num_divergent, all.missing = FALSE)
  checkmate::expect_integer(slog$num_max_treedepth, all.missing = FALSE)
  checkmate::expect_logical(slog$bad_ebfmi, all.missing = FALSE)

  slog_samp <- slog[slog$method == "sample", ]
  slog_gq <- slog[slog$method == "generate_quantities", ]

  expect_true(all(is.na(slog_gq$lp___mean)))
  checkmate::expect_double(slog_samp$lp___mean, all.missing = FALSE)
  checkmate::expect_double(slog_samp$lp___ess_tail, all.missing = FALSE)

  expect_no_match(names(slog), "theta")
  slog_theta <- stan_summary_log(
    "model",
    variables = c("lp__", "theta"),
    .recurse = TRUE)
  expect_identical(nrow(slog_theta), 3L)
  checkmate::expect_double(slog_theta$theta_mean, all.missing = FALSE)
  checkmate::expect_double(slog_theta$lp___rhat, all.missing = FALSE)

  slog_sf <- stan_summary_log(
    "model",
    summary_fns = list(~ posterior::quantile2(.x, probs = c(0.2, 0.4, 0.8))),
    .recurse = TRUE)
  checkmate::expect_double(slog_sf$lp___q20, all.missing = FALSE)
  checkmate::expect_double(slog_sf$lp___q40, all.missing = FALSE)
  checkmate::expect_double(slog_sf$lp___q80, all.missing = FALSE)
})

test_that("stan_summary_log() supports filtering by tag", {
  slog <- stan_summary_log(file.path("model", "stan"),
                           .include = "fxaInhibit1Ncp")
  expect_identical(slog$run, "fxa")
})

test_that("stan_add_summary() can extend run log", {
  rlog <- run_log("model", .recurse = TRUE)
  slog <- stan_add_summary(rlog)

  expect_identical(nrow(rlog), nrow(slog))
  expect_true(all(names(rlog) %in% names(slog)))
  expect_setequal(slog$model_type, c("nonmem", "nmbayes", "stan", "stan_gq"))
  checkmate::expect_double(slog$lp___mean, all.missing = FALSE)
  checkmate::expect_logical(slog$bad_ebfmi, all.missing = FALSE)

  slog <- run_log("model", .recurse = TRUE) %>%
    stan_add_summary(variables = "theta",
                     summary_fns = list(mean = mean))
  checkmate::expect_double(slog$theta_mean, all.missing = FALSE)
})

test_that("stan_add_summary(): variables=NULL uses all variables", {
  slog <- run_log(file.path("model", "stan"), .include = "bern") %>%
    stan_add_summary(variables = NULL,
                     summary_fns = list(mean = mean))
  fit <- read_fit_model(file.path("model", "stan", "bern"))
  expect_true(all(paste0(fit$metadata()$variables, "_mean") %in% names(slog)))
})

test_that("stan_summary_log(): summary_fns=list() selects no variables", {
  slog <- stan_summary_log(file.path("model", "stan"), summary_fns = list())
  checkmate::expect_integer(slog$num_divergent, all.missing = FALSE)
  expect_no_match(names(slog), "lp__")
})

test_that("stan_summary_log(): non-indexed name selects non-scalar values", {
  fit <- read_fit_model(file.path("model", "stan", "bern-gq"))
  bern_gq_vars <- paste0(fit$metadata()$variables, "_mean")

  slog <- stan_summary_log(file.path("model", "stan"),
                           summary_fns = "mean", variables = "y_rep")
  expect_true(all(bern_gq_vars %in% names(slog)))

  # Scalar can be pulled out with index.
  slog <- stan_summary_log(file.path("model", "stan"),
                           summary_fns = "mean", variables = "y_rep[1]")
  expect_true("y_rep[1]_mean" %in% names(slog))
  expect_false(any(setdiff(bern_gq_vars, "y_rep[1]_mean") %in% names(slog)))
})
