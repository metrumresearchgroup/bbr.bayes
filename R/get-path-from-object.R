#' @export
build_path_from_model.bbi_nmbayes_model <- function(...) {
  rlang::inform(
    c(
      "build_path_from_model() does not build paths to files in chain directories.",
      i = "Consider using chain_paths() instead."
    ),
    .frequency = "once",
    .frequency_id = "bbr.bayes/build_path_from_model->chain_paths"
  )
  NextMethod()
}

#' @export
get_config_path.bbi_nmbayes_model <- function(.bbi_object, .check_exists = TRUE) {
  .path <- file.path(
    get_output_dir(.bbi_object, .check_exists = .check_exists),
    "init", "bbi_config.json")

  if (isTRUE(.check_exists)) {
    checkmate::assert_file_exists(.path)
  }

  return(.path)
}

#' @export
get_data_path.bbi_nmbayes_model <- function(.bbi_object, ...) {
  mod_init_path <- file.path(get_output_dir(.bbi_object, .check_exists = FALSE), "init")
  if (!fs::file_exists(paste0(mod_init_path, ".yaml"))) {
    return(NextMethod())
  }

  return(get_data_path(read_model(mod_init_path), ...))
}

#' @export
get_model_path.bbi_stan_model <- function(.bbi_object, .check_exists = TRUE) {
  get_stan_path_impl(.bbi_object, STANMOD_SUFFIX, .check_exists)
}

#' @export
get_model_path.bbi_stan_summary <- function(.bbi_object, .check_exists = TRUE) {
  get_stan_path_impl(.bbi_object, STANMOD_SUFFIX, .check_exists)
}

#' @export
get_output_dir.bbi_stan_model <- function(.bbi_object, .check_exists = TRUE) {
  get_stan_path_impl(.bbi_object, STAN_OUTDIR_SUFFIX, .check_exists)
}

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
