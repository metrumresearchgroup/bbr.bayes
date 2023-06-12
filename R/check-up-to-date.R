
#' @export
check_up_to_date.bbi_nmbayes_model <- function(.bbi_object, ...) {
  res <- NextMethod()
  model_same <- res["model"]
  data_same <- res["data"]
  for (sub in get_chain_dirs(.bbi_object)) {
    res <- check_up_to_date(read_model(sub), ...)
    model_same <- model_same && res["model"]
    data_same <- data_same && res["data"]
  }

  return(invisible(c(model = model_same, data = data_same)))
}

#' Check that Stan model is up to date
#'
#' The details of which files are "model files" and which are "data files" are
#' detailed in the [bbr::check_up_to_date()] docs.
#'
#' * The model files:
#'   * `<run>.stan`
#'
#'   * `<run>-stanargs.R`
#'
#'   * `<run>-init.R`, if the model is _not_ a standalone generated quantities
#'     model
#'
#'   * `<run>-fitted-params.R`, if the model is a standalone generated
#'     quantities model (i.e. a model that inherits from `bbi_stan_gq_model`).
#'
#' * The data files (see `.build_data` argument):
#'
#'   * `<run>-standata.R`
#'
#'   * `<run>-standata.json`
#'
#' For standalone generated quantities, the `bbi_config.json` files in the
#' output directory of the models listed in the `gq_parent` fields are also
#' considered "data" files. If a `gq_parent` is re-submitted and anything
#' recorded in `bbi_config.json` changes, the linked "stan_gq" model is
#' considered out of date.
#'
#' @param .build_data If `TRUE`, the default, run `-standata.R` and save the
#'   output to a temp file and check the hash of _the temp file_ against the
#'   `bbi_config.json` hash. This option actually runs the code and,
#'   importantly, verifies that the input data to `-standata.R` has not changed
#'   either. If `FALSE`, check the hashes of the `-standata.R` and
#'   `-standata.json` against the hashes in `bbi_config.json` but do _not_ run
#'   the `-standata.R` script. This option is less secure and primarily exists
#'   for quicker checking if building the data is time consuming for certain
#'   models.
#' @inheritParams bbr::check_up_to_date
#' @name check_up_to_date_stan_model
NULL

#' @rdname check_up_to_date_stan_model
#' @export
check_up_to_date.bbi_stan_model <- function(.bbi_object, .build_data = TRUE, ...) {
  ellipsis::check_dots_empty()
  check_up_to_date_stan(.bbi_object, .build_data, gq = FALSE)
}

#' @rdname check_up_to_date_stan_model
#' @export
check_up_to_date.bbi_stan_gq_model <- function(.bbi_object, .build_data = TRUE, ...) {
  ellipsis::check_dots_empty()
  check_up_to_date_stan(.bbi_object, .build_data, gq = TRUE)
}

#' @rdname check_up_to_date_stan_model
#' @export
check_up_to_date.bbi_stan_summary <- function(.bbi_object, .build_data = TRUE, ...) {
  ellipsis::check_dots_empty()
  check_up_to_date_stan(.bbi_object, .build_data, gq = FALSE)
}

#' @rdname check_up_to_date_stan_model
#' @export
check_up_to_date.bbi_stan_gq_summary <- function(.bbi_object, .build_data = TRUE, ...) {
  ellipsis::check_dots_empty()
  check_up_to_date_stan(.bbi_object, .build_data, gq = TRUE)
}

#' Private implementation to check that a Stan model is up-to-date
#'
#' Specifically, check that required files on disk have not changed since the
#' model was run. This is accomplished by taking their md5 hashes and comparing
#' it to the hashes stored in `bbi_config.json`. The details of which files are
#' "model files" and which are "data files" are detailed in the
#' [bbr::check_up_to_date()] docs.
#'
#' @inheritParams bbr::check_up_to_date
#' @param gq Is the check for a standalone generated quantities model? If so,
#'   -init.R is dropped from the set of model files and -fitted-params.R is
#'   included.
#' @return A (named) logical vector of length 2. The first element (named
#'   `"model"`) refers to the model files mentioned above. The second element
#'   (named `"data"`) refers to the data files mentioned above. For both
#'   elements, they will be `TRUE` if nothing has changed, `FALSE` if anything
#'   has changed.
#' @noRd
check_up_to_date_stan <- function(.mod, .build_data = FALSE, gq = FALSE) {
  # check model and load config
  check_stan_model(.mod, .syntax = FALSE, .error = TRUE)

  config_path <- file.path(get_output_dir(.mod, .check_exists = FALSE), "bbi_config.json")
  if (!fs::file_exists(config_path)) {
    stop(glue("Cannot check if {get_model_id(.mod)} is up-to-date because it has not been run yet."))
  }
  config <- jsonlite::fromJSON(config_path)

  # check necessary files for changes
  stan_file <- get_model_path(.mod)
  args_file <- build_path_from_model(.mod, STANARGS_SUFFIX)
  model_files <- c(stan_file, args_file)

  data_r_file <- build_path_from_model(.mod, STANDATA_R_SUFFIX)
  data_json_file <- build_path_from_model(.mod, STANDATA_JSON_SUFFIX)
  temp_data_name <- as.character(
    glue("Running {basename(data_r_file)} produces different results"))
  data_files <- c(data_r_file, data_json_file, temp_data_name)

  changed_files <- config[[CONFIG_MODEL_MD5]] != tools::md5sum(stan_file)

  if (isTRUE(gq)) {
    fp_file <- build_path_from_model(.mod, STAN_FITTED_PARAMS_SUFFIX)
    changed_files <- c(
      changed_files,
      config[[STANCFG_FITTED_PARAMS_MD5]] != tools::md5sum(fp_file))
    model_files <- c(model_files, fp_file)

    gq_parent_name <- "bbi_config.json files in gq_parent models"
    data_files <- c(data_files, gq_parent_name)
    gq_changed <- !identical(config[[STANCFG_GQ_PARENT_MD5]],
                             unname(get_gq_parent_md5(.mod)))
    names(gq_changed) <- gq_parent_name
    changed_files <- c(changed_files, gq_changed)
  } else {
    init_file <- build_path_from_model(.mod, STANINIT_SUFFIX)
    changed_files <- c(
      changed_files,
      config[[STANCFG_INIT_MD5]] != tools::md5sum(init_file))
    model_files <- c(model_files, init_file)
  }

  changed_files <- c(
    changed_files,
    config[[STANCFG_ARGS_MD5]] != tools::md5sum(args_file)
  )

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
    model = !any(changed_files[model_files]),
    data = !any(changed_files[data_files], na.rm = TRUE)
  )

  return(invisible(res))
}

