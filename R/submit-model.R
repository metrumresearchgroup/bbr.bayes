
### NONMEM Bayes

#' Submit model based on a `bbi_nmbayes_model` object
#'
#' Model submission consists of two steps: generating the initial values
#' (`METHOD=CHAIN` run) and sampling for each chain (`METHOD=BAYES` or
#' `METHOD=NUTS` runs).
#'
#' TODO: Provide details about expected ctl lines.
#'
#' @param .dry_run Do not submit the sampling runs; just report what command
#'   would be executed via the returned object. **Note**: The METHOD=CHAIN model
#'   is executed to generate the initialization values regardless of this value.
#' @inheritParams bbr::submit_model
#' @export
submit_model.bbi_nmbayes_model <- function(
  .mod,
  .bbi_args = NULL,
  .mode = getOption("bbr.bbi_exe_mode"),
  ...,
  .overwrite = NULL,
  .config_path = NULL,
  .wait = TRUE,
  .dry_run = FALSE
  ) {
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

  # Convert model to bbi_nonmem_model for initialization. Another option would
  # be to call NextMethod(), but modifying arguments with that approach less
  # straightforward.
  mod_init <- .mod
  class(mod_init) <- class(mod_init)[-1]

  submit_model(
    mod_init,
    .bbi_args = .bbi_args,
    .overwrite = .overwrite,
    .config_path = .config_path,
    .wait = .wait,
    # Regardless of the mode for the main sampling (triggered by run_chains),
    # this upfront initialization should always be done locally.
    .mode = "local",
    .dry_run = FALSE)

  run_chains(.mod,
             .bbi_args = .bbi_args,
             .mode = .mode,
             ...,
             .config_path = .config_path,
             .wait = .wait,
             .dry_run = .dry_run)
}

#' Run Bayes chains
#'
#' Run multiple chains of a Bayes model after initial estimates have been
#' generated
#'
#' @param .mod A `bbi_nmbayes_model` object.
#' @param ... Arguments passed to [bbr::submit_model()].
#' @noRd
run_chains <- function(.mod, ...) {
  checkmate::assert_class(.mod, NMBAYES_MOD_CLASS)

  ctl <- readr::read_lines(get_model_path(.mod))

  row_bayes <- stringr::str_detect(ctl, "METHOD=BAYES|METHOD=NUTS")
  est_bayes <- ctl[row_bayes]
  est_bayes <- stringr::str_replace(est_bayes, "^;", "")
  ctl[row_bayes] <- est_bayes

  row_table <- stringr::str_detect(ctl, ";\\s*\\$TABLE")
  block_table <- ctl[row_table]
  block_table <- stringr::str_replace(block_table, "^;", "")
  ctl[row_table] <- block_table

  row_chain <- stringr::str_detect(ctl, "METHOD=CHAIN")
  est_chain <- ctl[row_chain]
  n_chain <- as.numeric(stringr::str_extract(est_chain, "(?<=NSAMPLE=)[0-9]+"))
  est_chain <- stringr::str_replace(est_chain, "NSAMPLE=[0-9]+", "NSAMPLE=0")
  est_chain <- stringr::str_replace(est_chain, "FILE=", "FILE=../")

  row_data <- stringr::str_detect(ctl, "\\$DATA")
  data_record <- ctl[row_data]
  ctl[row_data] <- stringr::str_replace(
    data_record,
    "\\$DATA\\s+",
    "$DATA ../")

  row_extrasend <- stringr::str_detect(ctl, "extrasend")
  ctl[row_extrasend] <- stringr::str_replace(
    ctl[row_extrasend],
    "extrasend",
    "../extrasend")

  .run <- get_model_id(.mod)
  outdir <- get_output_dir(.mod)
  mods <- purrr::map(seq_len(n_chain), function(.chain) {
    est_chain_i <- stringr::str_replace(
      est_chain,
      "ISAMPLE=0",
      glue("ISAMPLE={.chain}"))
    est_bayes_i <- stringr::str_replace(
      est_bayes,
      "SEED=[0-9]+",
      glue("SEED={.chain}"))
    ctl_i <- ctl
    ctl_i[row_chain] <- est_chain_i
    ctl_i[row_bayes] <- est_bayes_i
    readr::write_lines(ctl_i, file.path(
      outdir,
      glue("{.run}_{.chain}.ctl"))
    )

    new_model(
      file.path(outdir, glue("{.run}_{.chain}")),
      .description = glue("Chain {.chain}"),
    )
  })
  bbr::submit_models(mods, ...)
}

### Stan

#' Submit model based on a `bbi_stan_model` object
#'
#' The model is executed via [cmdstanr::sample()].
#'
#' @name stan_submit_model
#' @inheritParams bbr::submit_model
#' @param .mode Mode of model submission. Stan models currently only support
#'   local execution.
#' @param ... Additional arguments (ignored for all Stan models).
NULL

#' @rdname stan_submit_model
#' @export
submit_model.bbi_stan_model <- function(
  .mod,
  .mode = c("local"),
  ...,
  .overwrite = NULL
) {
  rlang::check_dots_empty()
  res <- submit_stan_model_cmdstanr(
    .mod,
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
                                       .mode = c("local"), # TODO: add sge mode for cmdstanr
                                       .overwrite = NULL) {

  # check against YAML
  check_yaml_in_sync(.mod)
  check_stan_model(.mod, .error = TRUE)

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

  stanmod <- compile_stanmod(.mod)

  # build args to pass to cmdstanr::sample()
  stanargs <- get_stanargs(.mod)
  check_reserved_stanargs(stanargs, method = "sample")

  if(is.null(stanargs$seed)) {
    stop("You must set a seed to run `submit_model()`. Use `set_stanargs(.mod, list(seed = <num>))` to set.", call. = FALSE)
  }

  cli::cli_h1("Running model with the following specified arguments")
  cli::cli_h3("(all other arguments will be cmdstan defaults)")
  print(stanargs)

  stanargs[["output_dir"]] <- get_output_dir(.mod)
  stanargs[["data"]] <- standata_json_path

  # construct input data set and initial estimates
  standata_list <- build_data(.mod, .out_path = standata_json_path)
  stanargs[["init"]] <- import_stan_init(.mod, .standata = standata_list, .stanargs = stanargs)
  rm(standata_list) # once we've passed this to import_stan_init() we don't need it in memory

  check_unknown_stanargs(stanargs, method = "sample")

  # launch model
  res <- do.call(
    stanmod$sample,
    args = stanargs
  )

  # if successful, save model and write bbi_config.json to disk
  save_fit_stanmod(res, build_path_from_model(.mod, STAN_MODEL_FIT_RDS))
  build_stan_bbi_config(.mod)

  return(res)
}
