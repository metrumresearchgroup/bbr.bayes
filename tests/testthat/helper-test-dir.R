
#' Create temporary directory for bbr.bayes tests
#'
#' This behaves like `withr::local_tempdir()`, but 1) changes the default name
#' of the temporary directory and 2) feeds the path to `normalizePath()` before
#' returning it. The latter is important because bbr normalizes paths in several
#' spots (e.g., new_model() processes `ABS_MOD_PATH` with `normalizePath()`),
#' causing the value to not match the result of `tempdir()` (e.g., if a path
#' contains a symlink).
#'
#' @param pattern,clean,.local_envir Arguments passed to
#'   `withr::local_tempdir()`.
local_test_dir <- function(pattern = "bbr-bayes-tests-", clean = TRUE,
                           .local_envir = parent.frame()) {
  tdir <- withr::local_tempdir(pattern = pattern, clean = clean,
                               .local_envir = .local_envir)
  return(normalizePath(tdir))
}

#' Create temporary directory with Stan model
#'
#' Call `local_test_dir()` and copy "bern" test model to `model/stan/bern`.
#'
#' @param clean,.local_envir Arguments passed to `withr::local_tempdir()`.
local_stan_bern_model <- function(clean = TRUE, .local_envir = parent.frame()) {
  tdir <- local_test_dir(clean = clean, .local_envir = .local_envir)
  mdir <- file.path(tdir, "model", "stan")
  fs::dir_create(mdir)
  copy_model_from(STAN_MOD3, file.path(mdir, "bern"))
  withr::local_dir(tdir, .local_envir = .local_envir)
}

#' Copy nmbayes model 1100 to temporary directory and delete `*.iph` files.
#'
#' @param clean,.local_envir Arguments passed to `withr::local_tempdir()`.
local_nmbayes_model_no_iph <- function(clean = TRUE, .local_envir = parent.frame()) {
  tdir <- local_test_dir(clean = clean, .local_envir = .local_envir)
  modfile <- get_model_path(NMBAYES_MOD1)
  fs::file_copy(modfile, tdir)
  fs::file_copy(get_yaml_path(NMBAYES_MOD1), tdir)

  mod_id <- get_model_id(NMBAYES_MOD1)
  rundir <- file.path(tdir, mod_id)
  fs::dir_copy(fs::path_ext_remove(modfile),
               rundir)

  mod <- read_model(rundir)
  iphs <- chain_paths_impl(mod, extension = ".iph", check_exists = "all")
  if (!all(fs::path_has_parent(iphs, get_model_working_directory(mod)))) {
    fail(glue("Returned files are not under expected directory",
              " - directory: {get_model_working_directory(mod)}",
              " - files:     {files}",
              files = paste(iphs, collapse = ", "),
              .sep = "\n"))
    return(NULL)
  }
  fs::file_delete(iphs)

  return(mod)
}
