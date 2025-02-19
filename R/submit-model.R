
### NONMEM Bayes

#' Submit model based on a `bbi_nmbayes_model` object
#'
#' Model submission consists of two steps: generating the initial values
#' (`METHOD=CHAIN` run) and sampling for each chain (`METHOD=BAYES` or
#' `METHOD=NUTS` runs).
#'
#' @name nmbayes_submit_model
#' @param .overwrite Whether to overwrite an existing output directory. Note
#'   that, unlike the submission method for regular `bbi_nonmem_model` objects,
#'   this method does not inspect `.bbi_args` or `bbi.yaml` when the value is
#'   `NULL`; any value other than `TRUE` is treated the same as `FALSE`.
#' @param .dry_run Do not submit the sampling runs; just report what command
#'   would be executed via the returned object. **Note**: The METHOD=CHAIN model
#'   is executed to generate the initialization values regardless of this value.
#' @inheritParams bbr::submit_model
#' @seealso [bbr_nmbayes] for a high-level description of how NONMEM Bayes
#'   models are structured
NULL

#' @rdname nmbayes_submit_model
#' @export
submit_model.bbi_nmbayes_model <- function(
  .mod,
  .bbi_args = NULL,
  .mode = getOption("bbr.bbi_exe_mode"),
  ...,
  .overwrite = FALSE,
  .config_path = NULL,
  .wait = TRUE,
  .dry_run = FALSE
  ) {
  if (!requireNamespace("nmrec", quietly = TRUE)) {
    stop("nmrec package is required to submit nmbayes model.")
  }

  .config_path <- if (is.null(.config_path)) {
    # Explicitly pass the default value because it's needed for the
    # METHOD={BAYES,NUTS} runs, which happen one level deeper.
    file.path(get_model_working_directory(.mod),
              "bbi.yaml")
  } else {
    # Ensure that user-specified values work from the METHOD={BAYES,NUTS}
    # subdirectory.
    fs::path_abs(.config_path)
  }

  outdir <- get_output_dir(.mod, .check_exists = FALSE)
  if (fs::dir_exists(outdir)) {
    if (!isTRUE(.overwrite)) {
      subdirs <- basename(fs::dir_ls(outdir, all = TRUE, type = "directory"))
      if (length(subdirs) && !identical(subdirs, "init")) {
        stop("Output for model ", get_model_id(.mod), " already exists.\n",
             "Pass `.overwrite = TRUE` to overwrite.")
      }
    }
    fs::dir_delete(outdir)
  }

  mod_init <- nmbayes_init(.mod)
  submit_model(
    mod_init,
    .bbi_args = .bbi_args,
    .overwrite = TRUE,
    .config_path = .config_path,
    .wait = .wait,
    # Regardless of the mode for the main sampling (triggered by run_chains),
    # this upfront initialization should always be done locally.
    .mode = "local",
    .dry_run = FALSE)

  if (!isTRUE(.dry_run)) {
    # Write top-level md5sum for check_up_to_date() because bbi_config.json
    # files will be present only for the submodels.
    jsonlite::write_json(
      list("model_md5" = jsonlite::unbox(tools::md5sum(get_model_path(.mod)))),
      path = file.path(get_output_dir(.mod), "nmbayes.json"))
  }

  run_chains(.mod,
             .bbi_args = .bbi_args,
             .mode = .mode,
             ...,
             .config_path = .config_path,
             .wait = .wait,
             .dry_run = .dry_run)
}

### Stan

#' Submit model based on a `bbi_stan_model` object
#'
#' @description
#'
#' Run the model via \pkg{cmdstanr}, selecting the `CmdStanModel` method based
#' the model type:
#'
#'  * execute the model with [$sample()][cmdstanr::model-method-sample] by
#'    default
#'
#'  * if object inherits from `bbi_stan_gq_model`, call
#'    [$generate_quantities()][cmdstanr::model-method-generate-quantities]
#'    instead
#'
#' @name stan_submit_model
#' @inheritParams bbr::submit_model
#' @param .bbi_args Unused argument (present for compatibility with
#'   [bbr::submit_model()]).
#' @param .mode Mode of model submission. Stan models currently only support
#'   local execution.
#' @param ... Additional arguments (ignored for all Stan models).
#' @param .overwrite Whether to overwrite an existing output directory. Note
#'   that, unlike the submission method for regular `bbi_nonmem_model` objects,
#'   this method does not inspect `.bbi_args` or `bbi.yaml` when the value is
#'   `NULL`; any value other than `TRUE` is treated the same as `FALSE`.
#' @seealso [bbr_stan] for a high-level description of how Stan models are
#'   structured
NULL

#' @rdname stan_submit_model
#' @export
submit_model.bbi_stan_model <- function(
  .mod,
  .bbi_args = NULL,
  .mode = c("local"),
  ...,
  .overwrite = FALSE
) {
  rlang::check_dots_empty()
  if (!is.null(.bbi_args)) {
    stop(".bbi_args must be NULL for model_type=stan")
  }
  res <- submit_stan_model_cmdstanr(
    .mod,
    "sample",
    .mode = .mode,
    .overwrite = .overwrite
  )
  return(res)
}

#' @rdname stan_submit_model
#' @export
submit_model.bbi_stan_gq_model <- function(.mod, .bbi_args = NULL, .mode = c("local"),
                                           ..., .overwrite = FALSE) {
  rlang::check_dots_empty()
  if (!is.null(.bbi_args)) {
    stop(".bbi_args must be NULL for model_type=stan_gq")
  }

  # Note: get_stan_gq_parent() will abort if any gq_parent lacks a YAML.
  gq_parent <- get_stan_gq_parent(.mod)
  if (!is.null(gq_parent)) {
    configs_exist <- fs::file_exists(build_config_paths(gq_parent))
    if (!all(configs_exist)) {
      stop("Run gq_parent first:\n",
           paste("  -", gq_parent[!configs_exist]), collapse = "\n")
    }
  }

  res <- submit_stan_model_cmdstanr(
    .mod,
    "generate_quantities",
    .mode = .mode,
    .overwrite = .overwrite
  )
  return(res)
}

#' Submit a Stan model via cmdstanr
#'
#' Private implementation function called by `submit_model()` dispatches.
#' @param .mod An S3 object of class `bbi_stan_model`
#' @return The object returned from [cmdstanr::sample()]
#' @noRd
submit_stan_model_cmdstanr <- function(.mod,
                                       .method = c("sample", "generate_quantities"),
                                       .mode = c("local"), # TODO: add sge mode for cmdstanr
                                       .overwrite = FALSE) {

  # check against YAML
  check_yaml_in_sync(.mod)
  # Use .syntax=FALSE because that's just feeding the model to stanc, and
  # $compile() will trigger that first thing.
  check_stan_model(.mod, .syntax = FALSE, .error = TRUE)

  # check for valid type arg
  .mode <- match.arg(.mode)

  out_dir <- get_output_dir(.mod, .check_exists = FALSE)
  standata_json_path <- build_path_from_model(.mod, STANDATA_JSON_SUFFIX)

  if (fs::dir_exists(out_dir)) {
    if (isTRUE(.overwrite)) {
      fs::dir_delete(out_dir)
      fs::dir_create(out_dir)
      if(fs::file_exists(standata_json_path)) { fs::file_delete(standata_json_path) }
    } else {
      stop(glue("{out_dir} already exists. Pass submit_model(..., .overwrite = TRUE) to delete it and re-run the model."), call. = FALSE)
    }
  } else {
    fs::dir_create(out_dir)
  }

  stanargs <- get_stanargs(.mod)
  check_reserved_stanargs(stanargs, method = .method)

  if(is.null(stanargs$seed)) {
    stop("You must set a seed to run `submit_model()`. Use `set_stanargs(.mod, list(seed = <num>))` to set.", call. = FALSE)
  }

  stanmod <- compile_stanmod(.mod)

  cli::cli_h1(glue("Calling ${.method} with the following specified arguments"))
  cli::cli_h3("(all other arguments will be cmdstan defaults)")
  print(stanargs)

  stanargs[["output_dir"]] <- get_output_dir(.mod)
  stanargs[["data"]] <- standata_json_path

  standata_list <- build_data(.mod, .out_path = standata_json_path)
  if (.method == "sample") {
    stanargs[["init"]] <- import_stan_init(.mod, .standata = standata_list, .stanargs = stanargs)
  } else {
    stanargs[["fitted_params"]] <- import_stan_fitted_params(.mod)
  }
  rm(standata_list) # once we've passed this to import_stan_init() we don't need it in memory

  check_unknown_stanargs(stanargs, method = .method)

  # launch model
  res <- do.call(stanmod[[.method]], args = stanargs)

  # if successful, save model and write bbi_config.json to disk
  save_fit_stanmod(res, build_path_from_model(.mod, STAN_MODEL_FIT_RDS))
  build_stan_bbi_config(.mod)

  return(res)
}
