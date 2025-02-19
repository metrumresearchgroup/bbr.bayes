
test_that("chain_paths() returns files in chain subdirs", {
  exts <- chain_paths(NMBAYES_MOD1, extension = ".ext")

  expect_length(exts, 2)
  expect_true(all(fs::path_has_parent(exts, NMBAYES_ABS_MODEL_DIR)))
  expect_setequal(basename(exts), c("1100-1.ext", "1100-2.ext"))

  # It doesn't matter if "." is included.
  expect_identical(exts, chain_paths(NMBAYES_MOD1, extension = "ext"))

  expect_setequal(basename(chain_paths(NMBAYES_MOD1, extension = ".phi")),
                  c("1100-1.phi", "1100-2.phi"))
})

test_that("chain_paths() accepts custom name", {
  res <- chain_paths(NMBAYES_MOD1,
                     name = bbr::get_model_id(NMBAYES_MOD1),
                     extension = ".ext")
  expect_length(res, 2)
  expect_true(all(fs::path_has_parent(res, NMBAYES_ABS_MODEL_DIR)))
  expect_setequal(basename(res), c("1100.ext", "1100.ext"))

  res <- chain_paths(NMBAYES_MOD1,
                     name = "foo",
                     extension = ".bar")
  expect_length(res, 2)
  expect_true(all(fs::path_has_parent(res, NMBAYES_ABS_MODEL_DIR)))
  expect_setequal(basename(res), c("foo.bar", "foo.bar"))

  # Empty name and extension can be used to grab chain directories.
  res <- chain_paths(NMBAYES_MOD1,
                     name = "",
                     extension = "")
  expect_length(res, 2)
  expect_true(all(fs::path_has_parent(res, NMBAYES_ABS_MODEL_DIR)))
  expect_setequal(basename(res), c("1100-1", "1100-2"))
})

test_that("chain_paths_impl() optionally checks existence: basic", {
  expect_length(chain_paths_impl(NMBAYES_MOD1,
                                 extension = ".ext",
                                 check_exists = "all"),
                2)
  expect_error(chain_paths_impl(NMBAYES_MOD1,
                                extension = ".i-do-not-exist",
                                check_exists = "all"),
               "Missing")
  expect_length(chain_paths_impl(NMBAYES_MOD1,
                                 extension = ".i-do-not-exist",
                                 check_exists = "no"),
                2)
  expect_identical(chain_paths_impl(NMBAYES_MOD1,
                                    extension = ".i-do-not-exist",
                                    check_exists = "all_or_none"),
                   character(0))
})

test_that("chain_paths_impl() optionally checks existence: some missing", {
  tdir <- local_test_dir()
  modfile <- get_model_path(NMBAYES_MOD1)
  fs::file_copy(modfile, tdir)
  fs::file_copy(get_yaml_path(NMBAYES_MOD1), tdir)

  mod_id <- get_model_id(NMBAYES_MOD1)
  rundir <- file.path(tdir, mod_id)
  fs::dir_copy(fs::path_ext_remove(modfile),
               rundir)

  mod <- read_model(rundir)
  exts <- chain_paths_impl(mod, extension = ".ext", check_exists = "all")
  expect_length(exts, 2)
  if (!all(fs::path_has_parent(exts, get_model_working_directory(mod)))) {
    fail(glue("Returned files are not under expected directory",
              " - directory: {get_model_working_directory(mod)}",
              " - files:     {files}",
              files = paste(exts, collapse = ", "),
              .sep = "\n"))
    # Quit early because we don't want to delete a file that's not where we
    # expect.
    return(NULL)
  }
  fs::file_delete(exts[1])
  expect_error(
    chain_paths_impl(mod, extension = ".ext", check_exists = "all"),
    "Missing")
  expect_error(
    chain_paths_impl(mod, extension = ".ext", check_exists = "all_or_none"),
    "Missing")
  expect_error(
    chain_paths_impl(mod, extension = ".ext", check_exists = "all_or_none"),
    "Missing")
})

test_that("rename_nm_as_rvar works", {
  cases <- list(
    list(input = "THETA3", want = "THETA[3]"),
    list(input = "THETA33", want = "THETA[33]"),
    list(input = "THETA01", want = "THETA[01]"),
    list(input = "OMEGA(1,1)", want = "OMEGA[1,1]"),
    list(input = "OMEGA(12,13)", want = "OMEGA[12,13]"),
    list(
      input = c("THETA55", "SIGMA(60,1)"),
      want = c("THETA[55]", "SIGMA[60,1]")
    )
  )
  for (case in cases) {
    expect_identical(
      rename_nm_as_rvar(!!case$input),
      !!case$want
    )
  }
})
