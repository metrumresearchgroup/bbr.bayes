testthat::skip_if_not_installed("nmrec")
skip_long_tests("skipping long-running nmbayes submit_model tests")

if (identical(Sys.getenv("METWORX_VERSION"), "")) {
  skip("test-workflow-nmbayes only runs on Metworx")
}

# TODO: Consider reworking this helper to be used elsewhere.
local_model_tempdir <- function(clean = TRUE, .local_envir = parent.frame()) {
  tdir <- local_test_dir(clean = clean, .local_envir = .local_envir)
  withr::local_dir(tdir, .local_envir = .local_envir)

  fs::dir_copy(
    system.file("extdata", package = "bbr.bayes", mustWork = TRUE),
    ".")

  mdir <- file.path("model", "nonmem", "bayes")
  fs::dir_create(mdir)
  bbr::bbi_init(mdir, "/opt/NONMEM", "nm75")

  parent_mod_path <- system.file(mdir, "1100",
                                 package = "bbr.bayes", mustWork = TRUE)
  fs::dir_copy(parent_mod_path, mdir)
  fs::file_copy(paste0(parent_mod_path, c(".yaml", ".ctl")),
                mdir)

  bbr::read_model(file.path(mdir, "1100")) %>%
    copy_model_from("1101") %>%
    bbr::update_model_id()
}

local_model_tempdir()

test_that("nmbayes: submit_model() works", {
  mod_path <- file.path("model", "nonmem", "bayes", "1101")
  mod <- read_model(mod_path)
  # Even with only two cores, this shouldn't take over two minutes.
  res <- submit_model(mod, .mode = "local")

  # This relays the return value of submit_models().
  expect_length(res, 1)
  expect_s3_class(res[[1]], "bbi_process")

  draws <- posterior::as_draws(mod)
  expect_identical(posterior::nchains(draws), 2L)
  expect_identical(posterior::niterations(draws), 50L)

  expect_true("MCMCOBJ_IPH[1,1]" %in% posterior::variables(draws))
  draws_no_iph <- posterior::as_draws(mod, include_iph = FALSE)
  expect_false("MCMCOBJ_IPH[1,1]" %in% posterior::variables(draws_no_iph))

  iph <- attr(draws, "nmbayes")$iph
  expect_s3_class(iph$id_map, "data.frame")
  expect_s3_class(iph$subpop_map, "data.frame")
})

test_that("nmbayes: run_log() captures runs correctly", {
  skip("FIXME: submodels confuse run_log()")
  log_df <- run_log(".")
  expect_identical(nrow(log_df), 2L)
  expect_identical(ncol(log_df), 10L)
  expect_setequal(basename(log_df[[ABS_MOD_PATH]]),
                  c("1100", "1101"))
})

test_that("nmbayes: summary_log() captures runs correctly", {
  skip("FIXME: submodels confuse summary_log()")
  withr::with_options(list(bbr.bbi_exe_path = bbr::read_bbi_path()), {
    log_df <- summary_log(".")
  })
  expect_identical(nrow(log_df), 2L)
  expect_setequal(basename(log_df[[ABS_MOD_PATH]]),
                  c("1100", "1101"))
})
