context("adding files to Stan models")

test_that("add_stanmod_file() works correctly for scaffold", {
  mod_name <- "testmod_add_stanmod_file1"
  suppressMessages(
    .m <- new_model(mod_name, .model_type = "stan")
  )
  on.exit(cleanup_model(.m))

  # check default
  fs::file_delete(get_model_path(.m))
  .m <- add_stanmod_file(.m)
  expect_equal(
    STANMOD_SCAFFOLD_MD5,
    as.character(tools::md5sum(get_model_path(.m)))
  )

  # check passing .source_file
  .sf <- get_model_path(STAN_MOD1)
  .m <- add_stanmod_file(.m, .source_file = .sf)
  expect_equal(
    as.character(tools::md5sum(.sf)),
    as.character(tools::md5sum(get_model_path(.m)))
  )
})

test_that("add_standata_file() works correctly for scaffold", {
  mod_name <- "testmod_add_standata_file1"
  suppressMessages(
    .m <- new_model(mod_name, .model_type = "stan")
  )
  on.exit(cleanup_model(.m))

  # check default
  fs::file_delete(build_path_from_model(.m, STANDATA_R_SUFFIX))
  .m <- add_standata_file(.m)
  expect_equal(
    STANDATA_SCAFFOLD_MD5,
    as.character(tools::md5sum(build_path_from_model(.m, STANDATA_R_SUFFIX)))
  )

  # check passing .source_file
  .sf <- build_path_from_model(STAN_MOD1, STANDATA_R_SUFFIX)
  .m <- add_standata_file(.m, .source_file = .sf)
  expect_equal(
    as.character(tools::md5sum(.sf)),
    as.character(tools::md5sum(build_path_from_model(.m, STANDATA_R_SUFFIX)))
  )
})

test_that("add_staninit_file() works correctly for scaffold", {
  mod_name <- "testmod_add_staninit_file1"
  suppressMessages(
    .m <- new_model(mod_name, .model_type = "stan")
  )
  on.exit(cleanup_model(.m))

  # check default
  .m <- add_staninit_file(.m)
  expect_true(any(grepl(
    "return\\(NULL\\)",
    readLines(build_path_from_model(.m, STANINIT_SUFFIX))
  )))

  # check passing .source_file
  .sf <- build_path_from_model(STAN_MOD1, STANINIT_SUFFIX)
  .m <- add_staninit_file(.m, .source_file = .sf)
  expect_equal(
    as.character(tools::md5sum(.sf)),
    as.character(tools::md5sum(build_path_from_model(.m, STANINIT_SUFFIX)))
  )
})

test_that("add_staninit_file() aborts on bbi_stan_gq_model", {
  tdir <- local_test_dir()
  m <- new_model(file.path(tdir, "stanmod"), .model_type = "stan_gq")
  expect_error(add_staninit_file(m), "stan_gq")
})

test_that("add_stan_fitted_params_file() works correctly for scaffold", {
  tdir <- local_test_dir()
  m <- new_model(file.path(tdir, "stanmod"), .model_type = "stan_gq")
  add_stan_fitted_params_file(m)
  expect_match(
    readLines(build_path_from_model(m, STAN_FITTED_PARAMS_SUFFIX)),
    "make_fitted_params",
    fixed = TRUE, all = FALSE)

  sf <- file.path(tdir, "source")
  cat("custom\n", file = sf)
  add_stan_fitted_params_file(m, .source_file = sf)
  expect_identical(
    readLines(build_path_from_model(m, STAN_FITTED_PARAMS_SUFFIX)),
    "custom")
})

test_that("add_stan_fitted_params_file() aborts on bbi_stan_model", {
  tdir <- local_test_dir()
  m <- new_model(file.path(tdir, "stanmod"), .model_type = "stan")
  expect_error(add_stan_fitted_params_file(m), "bbi_stan_gq_model")
})
