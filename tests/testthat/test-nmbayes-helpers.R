
test_that("get_chain_files() returns files chain subdirs", {
  exts <- get_chain_files(NMBAYES_MOD1, ".ext")

  expect_length(exts, 2)
  expect_true(all(fs::path_has_parent(exts, NMBAYES_ABS_MODEL_DIR)))
  expect_setequal(basename(exts), c("1100_1.ext", "1100_2.ext"))

  # It doesn't matter if "." is included.
  expect_identical(exts, get_chain_files(NMBAYES_MOD1, "ext"))

  expect_setequal(basename(get_chain_files(NMBAYES_MOD1, ".phi")),
                  c("1100_1.phi", "1100_2.phi"))
})

test_that("get_chain_files() optionally checks existence: basic", {
  expect_length(get_chain_files(NMBAYES_MOD1, ".ext",
                                check_exists = "all"),
                2)
  expect_error(get_chain_files(NMBAYES_MOD1, ".i-do-not-exist",
                               check_exists = "all"),
               "Missing")
  expect_length(get_chain_files(NMBAYES_MOD1, ".i-do-not-exist",
                                check_exists = "no"),
                2)
  expect_identical(get_chain_files(NMBAYES_MOD1, ".i-do-not-exist",
                                   check_exists = "all_or_none"),
                   character(0))
})

test_that("get_chain_files() optionally checks existence: some missing", {
  tdir <- local_test_dir()
  modfile <- get_model_path(NMBAYES_MOD1)
  fs::file_copy(modfile, tdir)
  fs::file_copy(get_yaml_path(NMBAYES_MOD1), tdir)

  mod_id <- get_model_id(NMBAYES_MOD1)
  rundir <- file.path(tdir, mod_id)
  fs::dir_copy(fs::path_ext_remove(modfile),
               rundir)

  mod <- read_model(rundir)
  exts <- get_chain_files(mod, ".ext", check_exists = "all")
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
  expect_error(get_chain_files(mod, ".ext", check_exists = "all"),
               "Missing")
  expect_error(get_chain_files(mod, ".ext", check_exists = "all_or_none"),
               "Missing")
})
