# This file contains copies of functions from bbr's setup-workflow-ref.R.
#
# TODO: Eliminate the need for these duplicate functions by reworking the tests
# to use temporary directories.

perturb_file <- function(path, txt = "foo", envir = parent.frame()) {
  checkmate::assert_string(path)
  original <- readr::read_file(path)
  readr::write_lines(txt, path, append = TRUE)
  withr::defer(readr::write_file(original, path), envir)
}

cleanup_model <- function(.mod) {
  if (fs::file_exists(get_yaml_path(.mod, .check_exists = FALSE)))  fs::file_delete(get_yaml_path(.mod))
  if (fs::file_exists(get_model_path(.mod, .check_exists = FALSE))) fs::file_delete(get_model_path(.mod))
  if (fs::dir_exists(get_output_dir(.mod, .check_exists = FALSE)))  fs::dir_delete(get_output_dir(.mod))
  if (fs::dir_exists(.mod[[ABS_MOD_PATH]]))  fs::dir_delete(.mod[[ABS_MOD_PATH]])
  rm(.mod)
}
