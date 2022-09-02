context("reading fit model objects from disk")

test_that("read_fit_model.character works correctly for Stan", {
  res <- read_fit_model(STAN_MOD1_PATH)
  expect_true(inherits(res, STAN_FIT_CLASS))

  # verify the sampler_diagnostics() method works
  smp <- res$sampler_diagnostics()
  expect_true(inherits(smp, STAN_SMP_DIAG_CLASS))
  expect_equal(dim(smp), STAN_SMP_DIAG_DIM)
})

test_that("read_fit_model.bbi_stan_model works correctly", {
  res <- read_fit_model(STAN_MOD1)
  expect_true(inherits(res, STAN_FIT_CLASS))

  # verify the sampler_diagnostics() method works
  smp <- res$sampler_diagnostics()
  expect_true(inherits(smp, STAN_SMP_DIAG_CLASS))
  expect_equal(dim(smp), STAN_SMP_DIAG_DIM)
})

test_that("read_fit_model adjusts output files to be in model directory", {
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
