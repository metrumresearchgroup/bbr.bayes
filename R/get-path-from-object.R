
#' @rdname get_path_from_object
#' @export
get_model_path.bbi_stan_model <- function(.bbi_object, .check_exists = TRUE) {
  get_stan_path_impl(.bbi_object, STANMOD_SUFFIX, .check_exists)
}

#' @rdname get_path_from_object
#' @export
get_model_path.bbi_stan_summary <- function(.bbi_object, .check_exists = TRUE) {
  get_stan_path_impl(.bbi_object, STANMOD_SUFFIX, .check_exists)
}

#' @rdname get_path_from_object
#' @export
get_output_dir.bbi_stan_model <- function(.bbi_object, .check_exists = TRUE) {
  get_stan_path_impl(.bbi_object, STAN_OUTDIR_SUFFIX, .check_exists)
}

#' @rdname get_path_from_object
#' @export
get_output_dir.bbi_stan_summary <- function(.bbi_object, .check_exists = TRUE) {
  get_stan_path_impl(.bbi_object, STAN_OUTDIR_SUFFIX, .check_exists)
}

#' @keywords internal
get_stan_path_impl <- function(.bbi_object, .suffix, .check_exists = TRUE) {
  .path <- build_path_from_model(.bbi_object, .suffix)

  if (isTRUE(.check_exists)) {
    if (fs::is_dir(.path)) {
      checkmate::assert_directory_exists(.path)
    } else {
      checkmate::assert_file_exists(.path)
    }
  }

  return(.path)
}
