#' Check Stan model for required files and syntax errors
#'
#' Ensure that a `bbi_stan_model` object has all the files necessary to submit
#' the model (see `?`[bbr_stan] for details). By default, also check the syntax
#' of that the `.stan` file.
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
#' @param .syntax Check the syntax of the model by calling
#'   [CmdStanModel$check_syntax()][cmdstanr::model-method-check_syntax].
#' @param .error If `FALSE`, the default, display a message for any problems
#'   found. If `TRUE`, signal an error.
#' @return Invisibly return `FALSE` if any problems are found (only relevant
#'   when `.error` is `FALSE`) and `TRUE` otherwise.
#' @export
check_stan_model <- function(.mod, .syntax = TRUE, .error = FALSE) {
  UseMethod("check_stan_model")
}

#' @export
check_stan_model.bbi_stan_model <- function(.mod, .syntax = TRUE, .error = FALSE) {
  check_stan_model_impl(.mod,
                        .syntax = .syntax,
                        .error = .error,
                        req_files = STAN_MODEL_REQ_FILES)
}

#' @export
check_stan_model.bbi_stan_gq_model <- function(.mod, .syntax = TRUE, .error = FALSE) {
  check_stan_model_impl(.mod,
                        .syntax = .syntax,
                        .error = .error,
                        req_files = STAN_GQ_MODEL_REQ_FILES)
}

check_stan_model_impl <- function(.mod, .syntax, .error, req_files) {
  ok <- check_stan_files(.mod, .error, req_files)

  # If the .stan file doesn't exist, check_stan_files() has already let the user
  # know about it.
  stanfile <- get_model_path(.mod, .check_exists = FALSE)
  if (isTRUE(.syntax) && isTRUE(fs::file_exists(stanfile))) {
    stanmod <- cmdstanr::cmdstan_model(stanfile, compile = FALSE)

    if (isTRUE(.error)) {
      check_syntax <- stanmod$check_syntax
    } else {
      check_syntax <- function(...) {
        tryCatch(
          stanmod$check_syntax(...),
          error = function(e) {
            # Calling conditionMessage() drops the "Error:" prefix.
            message(conditionMessage(e))
            return(FALSE)
          })
      }
    }
    ok_syntax <- check_syntax(quiet = TRUE)
    ok <- ok && ok_syntax
  }

  return(invisible(ok))
}

check_stan_files <- function(.mod, .error, req_files) {
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
