#' Build model input data
#'
#' Some types of models carry around code that builds the input data for the
#' modeling, instead of relying on a single static file on disk. When necessary,
#' **this function is called internally by [bbr::submit_model()]** to build the
#' data for running the model, but they can also be called manually by users for
#' other purposes.
#'
#' @details **Currently, only `bbi_stan_model` objects have this implemented.**
#' The `<run>-standata.R` file contains the code for building the Stan input
#' data.
#'
#' @return
#' Invisibly returns the data object returned from the relevant build function.
#' Also optionally saves the object to disk if a valid file path is passed to
#' `.out_path`.
#'
#' The data object is returned invisibly because sometimes you will want to call
#' this function solely for the side effect of writing the data to disk.
#'
#' @param .mod a `bbi_{.model_type}_model` object.
#' @param .out_path If `NULL`, the default, does not write any data to disk.
#'   Otherwise, pass a file path where the resulting data object should be
#'   written.
#' @param ... Arguments passed through to methods (currently none).
#'
#' @export
build_data <- function(.mod, .out_path = NULL, ...) {
  UseMethod("build_data")
}

#' @describeIn build_data executes the function in `<run>-standata.R` and
#'   returns the list that will be passed to the `data` argument of
#'   `cmdstanr::sample()`. Also optionally writes the returned data list to json
#'   with [cmdstanr::write_stan_json()] if an `.out_path` is passed.
#'
#' @export
build_data.bbi_stan_model <- function(.mod, .out_path = NULL, ...) {
  # source and call function
  standata_r_path <- build_path_from_model(.mod, STANDATA_R_SUFFIX)
  make_standata <- safe_source_function(standata_r_path, "make_standata")
  standata_list <- safe_call_sourced(
    .func = make_standata,
    .args = list(.dir = dirname(get_output_dir(.mod, .check_exists = FALSE))),
    .file = standata_r_path,
    .expected_class = "list"
  )

  # optionally write to json
  if (!is.null(.out_path)) {
    checkmate::assert_string(.out_path)
    if (!stringr::str_detect(.out_path, ".json$")) {
      stop(glue("build_data.bbi_stan_model(.out_path) must end in '.json' because a JSON file will be written. Got {.out_path}"), .call = FALSE)
    }
    cmdstanr::write_stan_json(standata_list, .out_path)
  }

  return(invisible(standata_list))
}
