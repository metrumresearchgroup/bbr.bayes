
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
