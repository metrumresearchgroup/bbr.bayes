#' Attaches a file to a model
#'
#' These functions take a model object and create the appropriate file in the
#' required location. If `.source_file` argument is used, this file will be
#' copied to the location. Otherwise, a template "scaffold" of the required file
#' is created in that location. **Note, this primarily intended for Stan
#' models** which require several necessary files to run. (See `?`[bbi_stan_model]
#' for details about these files.) Users can call `check_stan_model(.mod)` to
#' see if any of these files are missing.
#' @param .mod a `bbi_{.model_type}_model` object
#' @param .source_file If `NULL`, the default, create an empty scaffold file
#'   at the destination path. If not `NULL`, pass a path to a file that
#'   will be copied to the destination path. Use this if you have a
#'   file elsewhere on disk that you would like to use for this model.
#' @name add_file_to_model_dir
NULL

#' @describeIn add_file_to_model_dir Adds a `<run>.stan` model file.
#' @export
add_stanmod_file <- function(.mod, .source_file = NULL) {
  add_file_to_model_dir_impl(
    .mod,
    STAN_MOD_CLASS,
    STANMOD_SUFFIX,
    STANMOD_SCAFFOLD,
    .source_file
  )
}

#' @describeIn add_file_to_model_dir Adds a `<run>-standata.R` file for building the data. See `?`[bbi_stan_model] for details.
#' @export
add_standata_file <- function(.mod, .source_file = NULL) {
  add_file_to_model_dir_impl(
    .mod,
    STAN_MOD_CLASS,
    STANDATA_R_SUFFIX,
    STANDATA_SCAFFOLD,
    .source_file
  )
}

#' @describeIn add_file_to_model_dir Adds a `<run>-init.R` file for building the initial values. See `?`[bbi_stan_model] for details.
#' @export
add_staninit_file <- function(.mod, .source_file = NULL) {
  if (inherits(.mod, "bbi_stan_gq_model")) {
    stop("*-init.R file not applicable to stan_gq models")
  }
  add_file_to_model_dir_impl(
    .mod,
    STAN_MOD_CLASS,
    STANINIT_SUFFIX,
    STANINIT_SCAFFOLD,
    .source_file
  )
}

#' @describeIn add_file_to_model_dir Adds a `<run>-fitted-params.R` file that
#'   prepares previously generated MCMC samples as input for a standalone
#'   generated quantities run. See `?`[bbi_stan_model] for details.
#' @export
add_stan_fitted_params_file <- function(.mod, .source_file = NULL) {
  add_file_to_model_dir_impl(
    .mod,
    STAN_GQ_MOD_CLASS,
    STAN_FITTED_PARAMS_SUFFIX,
    STAN_FITTED_PARAMS_SCAFFOLD,
    .source_file
  )
}
###################################
# PRIVATE IMPLEMENTATION FUNCTIONS
###################################

#' Attaches a file to a model
#'
#' Implementation function for [add_file_to_model_dir()].
#'
#' @inheritParams add_file_to_model_dir
#' @param .model_class Function will assert that `.mod` inherits from this class.
#' @param .file_suffix Destination path is created with `build_path_from_model(.mod, .file_suffix)`
#' @param .scaffold If `.source_file` is `NULL`, the default, write
#'   `inst/stan-scaffolds/{.scaffold}` to the destination path.
#' @noRd
add_file_to_model_dir_impl <- function(
  .mod,
  .model_class,
  .file_suffix,
  .scaffold,
  .source_file = NULL
) {
  checkmate::assert_class(.mod, .model_class)

  dest_path <- build_path_from_model(.mod, .file_suffix)

  # copy over .source_file if one was passed in
  if(!is.null(.source_file)) {
    checkmate::assert_string(.source_file)

    fs::file_copy(.source_file, dest_path, overwrite = TRUE)
    message(glue("Copied {.source_file} to {dest_path}"))
    return(invisible(.mod))
  }

  # write scaffold to file, first checking if a non-scaffold file would be overwritten
  .scaffold <- system.file("stan-scaffolds", .scaffold,
                           package = "bbr.bayes", mustWork = TRUE)
  if (fs::file_exists(dest_path)) {
    if (tools::md5sum(.scaffold) == tools::md5sum(dest_path)) {
      return(invisible(.mod))
    }
    stop("File already exists at ", dest_path, call. = FALSE)
  }
  fs::file_copy(.scaffold, dest_path)

  # return invisibly so this will work in pipes
  return(invisible(.mod))
}

#' Private helper to add scaffolds of any missing stan files
#'
#' This is used when setting up a new model.
#'
#' @param .mod a `bbi_stan_model` or `bbi_stan_gq_model`
#' @keywords internal
scaffold_missing_stan_files <- function(.mod) {
  if (inherits(.mod, STAN_GQ_MOD_CLASS)) {
    req_files <- STAN_GQ_MODEL_REQ_FILES
  } else if (inherits(.mod, STAN_MOD_CLASS)) {
    req_files <- STAN_MODEL_REQ_FILES
  } else {
    stop(sprintf(".mod must be bbi_stan_model subclass, got %s",
                 deparse(class(.mod))))
  }

  files_to_check <- build_path_from_model(.mod, req_files)
  missing_files <- req_files[!fs::file_exists(files_to_check)]

  SCAFFOLD_LOOKUP <- rlang::list2(
    !!STANMOD_SUFFIX            := add_stanmod_file,
    !!STANARGS_SUFFIX           := stanargs_scaffold,
    !!STANDATA_R_SUFFIX         := add_standata_file,
    !!STANINIT_SUFFIX           := add_staninit_file,
    !!STAN_FITTED_PARAMS_SUFFIX := add_stan_fitted_params_file
  )

  purrr::walk(missing_files, function(.f) {
    message(glue("Automatically adding scaffolded {.f} file"))
    SCAFFOLD_LOOKUP[[.f]](.mod)
  })

  return(invisible(NULL))
}

#' Creates a starter -stanargs.R with empty list
#'
#' @param .mod a `bbi_stan_model`
stanargs_scaffold <- function(.mod) {
  set_stanargs(.mod, .stanargs = list(), .clear = TRUE)
}
