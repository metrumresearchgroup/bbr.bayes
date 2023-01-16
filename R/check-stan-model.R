#' Checks a `bbi_stan_model` for necessary files
#'
#' Checks a `bbi_stan_model` object to make sure it can find all of the files
#' necessary to submit the model. (See `?`[bbr_stan] for details about these
#' files.)
#'
#' @details
#' Will look for the following:
#'
#' * `build_path_from_model(.mod, "-standata.R")`
#'
#' * `build_path_from_model(.mod, "-stanargs.R")`
#'
#' * `build_path_from_model(.mod, ".stan")`
#'
#' * `build_path_from_model(.mod, "-init.R")`, unless it is a generated
#'   quantities model (i.e. a `bbi_stan_gq_model` subclass).
#'
#' * `build_path_from_model(.mod, "-fitted-params.R")`, if it is a generated
#'   quantities model (i.e. a `bbi_stan_gq_model` subclass).
#'
#' @param .mod A [bbi_stan_model] object
#' @param .error If `FALSE`, the default, will warn if any necessary files are
#'   missing. If `TRUE` will error instead.
#' @export
check_stan_model <- function(.mod, .error = FALSE) {
  UseMethod("check_stan_model")
}

#' @export
check_stan_model.bbi_stan_model <- function(.mod, .error = FALSE) {
  check_stan_model_impl(.mod,
                        .error = .error,
                        req_files = STAN_MODEL_REQ_FILES)
}

#' @export
check_stan_model.bbi_stan_gq_model <- function(.mod, .error = FALSE) {
  check_stan_model_impl(.mod,
                        .error = .error,
                        req_files = STAN_GQ_MODEL_REQ_FILES)
}

check_stan_model_impl <- function(.mod, .error, req_files) {
  # check if output dir exists and if not create an empty one
  model_dir <- dirname(get_output_dir(.mod, .check_exists = FALSE))
  if (!fs::dir_exists(model_dir)) fs::dir_create(model_dir)

  # check for files in output dir
  files_missing <- !fs::file_exists(build_path_from_model(.mod, req_files))
  problems <- NULL
  if (any(files_missing)) {
    missing_err_msg <- sprintf(MISSING_STAN_FILES_ERR_MSG, class(.mod)[1])
    problems <- paste(
      problems,
      glue("{missing_err_msg} from {get_model_id(.mod)}:"),
      paste(paste0(" * ", names(which(files_missing))), collapse = "\n"),
      paste("See `?add_file_to_model_dir` for helper functions to add them.\n"),
      sep = "\n"
    )
  }

  # checking if any of the files found are only scaffolds
  files_to_check <- build_path_from_model(.mod, req_files)
  scaffold_bool <- purrr::map_lgl(files_to_check, function(.f) {
    tools::md5sum(.f) %in% STAN_SCAFFOLD_MD5_VEC
  })

  if (any(scaffold_bool)) {
    scaffold_err_msg <- sprintf(STAN_SCAFFOLD_ERR_MSG, class(.mod)[1])
    problems <- paste(
      problems,
      scaffold_err_msg,
      paste(paste0(" * ", files_to_check[scaffold_bool]), collapse = "\n"),
      "Please add necessary code to the scaffolded files.\n",
      sep = "\n"
    )
  }

  if (!is.null(problems)) {
    if (isTRUE(.error)) {
      stop(problems, call. = FALSE)
    } else {
      message(problems)
    }
  }

  return(invisible(is.null(problems)))
}
