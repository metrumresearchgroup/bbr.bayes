context("reading fit model objects from disk")

### NONMEM Bayes

test_that("nmbayes: read_fit_model.character() works correctly", {
  res <- read_fit_model(NMBAYES_MOD1_PATH)
  expect_s3_class(res, "draws")

  res <- read_fit_model(NMBAYES_MOD1_PATH,
                        format = "df",
                        include_iph = FALSE)
  expect_s3_class(res, "draws_df")
  expect_false("MCMCOBJ_IPH[1,1]" %in% posterior::variables(res))
})

test_that("nmbayes: read_fit_model() works correctly", {
  res <- read_fit_model(NMBAYES_MOD1)
  expect_s3_class(res, "draws")
})

test_that("read_fit_model accepts draws format argument for nmbayes", {
  cases <- list(list(format = "array",
                     fn = posterior::as_draws_array),
                list(format = "df",
                     fn = posterior::as_draws_df),
                list(format = "matrix",
                     fn = posterior::as_draws_matrix),
                list(format = "list",
                     fn = posterior::as_draws_list),
                list(format = "rvars",
                     fn = posterior::as_draws_rvars))
  for (case in cases) {
    expect_equal(read_fit_model(NMBAYES_MOD1, format = !!case$format),
                 case$fn(NMBAYES_MOD1))
  }
})

### Stan

test_that("stan: read_fit_model.character() works correctly", {
  res <- read_fit_model(STAN_MOD1_PATH)
  expect_true(inherits(res, STAN_FIT_CLASS))

  # verify the sampler_diagnostics() method works
  smp <- res$sampler_diagnostics()
  expect_true(inherits(smp, STAN_SMP_DIAG_CLASS))
  expect_equal(dim(smp), STAN_SMP_DIAG_DIM)
})

test_that("stan: read_fit_model() works correctly", {
  res <- read_fit_model(STAN_MOD1)
  expect_true(inherits(res, STAN_FIT_CLASS))

  # verify the sampler_diagnostics() method works
  smp <- res$sampler_diagnostics()
  expect_true(inherits(smp, STAN_SMP_DIAG_CLASS))
  expect_equal(dim(smp), STAN_SMP_DIAG_DIM)
})

test_that("stan: read_fit_model() adjusts output files to be in model directory", {
  tdir <- withr::local_tempdir("bbr.bayes-read-model-test")
  mdir <- file.path(tdir, STAN_MOD_ID)
  fs::dir_copy(STAN_MOD1_PATH, mdir)
  fs::file_copy(file.path(STAN_ABS_MODEL_DIR, paste0(STAN_MOD_ID, ".yaml")),
                tdir)

  files <- read_fit_model(mdir)$output_files()
  expect_true(all(fs::path_has_parent(files, mdir)))
  expect_identical(basename(files),
                   basename(read_fit_model(STAN_MOD1)$output_files()))
})
