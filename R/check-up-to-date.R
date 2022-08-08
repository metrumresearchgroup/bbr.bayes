
#' @rdname check_up_to_date
#' @param .build_data **Only relevant to Stan models.** If `TRUE`, the default,
#'   run `-standata.R` and save the output to a temp file and check the hash of
#'   _the temp file_ against the `bbi_config.json` hash. This option actually
#'   runs the code and, importantly, verifies that the input data to
#'   `-standata.R` has not changed either. If `FALSE`, check the hashes of the
#'   `-standata.R` and `-standata.json` against the hashes in `bbi_config.json`
#'   but do _not_ run the `-standata.R` script. This option is less secure and
#'   primarily exists for quicker checking if building the data is time
#'   consuming for certain models.
#' @export
check_up_to_date.bbi_stan_model <- function(.bbi_object, .build_data = TRUE, ...) {
  ellipsis::check_dots_empty()
  check_up_to_date_stan(.bbi_object, .build_data)
}

#' @rdname check_up_to_date
#' @export
check_up_to_date.bbi_stan_summary <- function(.bbi_object, .build_data = TRUE, ...) {
  ellipsis::check_dots_empty()
  check_up_to_date_stan(.bbi_object, .build_data)
}

#' Private implementation to check that a Stan model is up-to-date
#'
#' Specifically, check that required files on disk have not changed since the
#' model was run. This is accomplished by taking their md5 hashes and comparing
#' it to the hashes stored in `bbi_config.json`. The details of which files are
#' "model files" and which are "data files" are detailed in the
#' [check_up_to_date()] docs.
#'
#' @inheritParams check_up_to_date
#'
#' @return A (named) logical vector of length 2. The first element (named
#'   `"model"`) refers to the model files mentioned above. The second element
#'   (named `"data"`) refers to the data files mentioned above. For both
#'   elements, they will be `TRUE` if nothing has changed, `FALSE` if anything
#'   has changed.
#' @keywords internal
check_up_to_date_stan <- function(.mod, .build_data = FALSE) {

  # check model and load config
  check_stan_model(.mod, .error = TRUE)

  config_path <- file.path(get_output_dir(.mod, .check_exists = FALSE), "bbi_config.json")
  if (!fs::file_exists(config_path)) {
    stop(glue("Cannot check if {get_model_id(.mod)} is up-to-date because it has not been run yet."))
  }
  config <- jsonlite::fromJSON(config_path)

  # check necessary files for changes
  changed_files <- c()

  stan_file <- get_model_path(.mod)
  changed_files <- c(
    changed_files,
    config[[CONFIG_MODEL_MD5]] != tools::md5sum(stan_file)
  )

  init_file <- build_path_from_model(.mod, STANINIT_SUFFIX)
  changed_files <- c(
    changed_files,
    config[[STANCFG_INIT_MD5]] != tools::md5sum(init_file)
  )

  args_file <- build_path_from_model(.mod, STANARGS_SUFFIX)
  changed_files <- c(
    changed_files,
    config[[STANCFG_ARGS_MD5]] != tools::md5sum(args_file)
  )

  data_r_file <- build_path_from_model(.mod, STANDATA_R_SUFFIX)
  data_json_file <- build_path_from_model(.mod, STANDATA_JSON_SUFFIX)
  temp_data_name <- as.character(glue("Running {basename(data_r_file)} produces different results"))
  if (isTRUE(.build_data)) {
    # if building data, run -standata.R and write output to temp file, then check that file
    temp_data_path <- fs::path_ext_set(tempfile(), ".json")
    suppressMessages(
      build_data(.mod, .out_path = temp_data_path)
    )

    changed_files <- c(
      changed_files,
      config[[CONFIG_DATA_MD5]] != tools::md5sum(data_json_file),
      config[[CONFIG_DATA_MD5]] != tools::md5sum(temp_data_path)
    )
    names(changed_files)[length(changed_files)] <- temp_data_name
  } else {
    changed_files <- c(
      changed_files,
      config[[STANCFG_DATA_MD5]] != tools::md5sum(data_r_file),
      config[[CONFIG_DATA_MD5]] != tools::md5sum(data_json_file)
    )
  }

  any_changes <- any(changed_files)

  if(isTRUE(any_changes)) {
    message(paste(
      glue("The following files have changed in {get_model_id(.mod)}"),
      paste("*", names(which(changed_files)), collapse = "\n"),
      sep = "\n"
    ))
  }

  # build return value
  res <- c(
    model = !any(changed_files[c(stan_file, init_file, args_file)]),
    data = !any(changed_files[c(data_r_file, data_json_file, temp_data_name)], na.rm = TRUE)
  )

  return(invisible(res))
}

