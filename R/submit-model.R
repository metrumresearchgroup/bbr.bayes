
#' @describeIn submit_model Takes a `bbi_stan_model` object. All arguments
#'   passed through `...` will be passed to [cmdstanr::sample()] method.
#' @export
submit_model.bbi_stan_model <- function(
  .mod,
  .bbi_args = NULL,
  .mode = c("local"),
  ...,
  .overwrite = NULL,
  .config_path = NULL,
  .wait = NULL,
  .dry_run=NULL
) {

  if(!is.null(.bbi_args))    {warning(".bbi_args is not a valid argument for submit_model.bbi_stan_model. Ignoring...")}
  if(!is.null(.config_path)) {warning(".config_path is not a valid argument for submit_model.bbi_stan_model. Ignoring...")}
  if(!is.null(.wait))        {warning(".wait is not a valid argument for submit_model.bbi_stan_model. Ignoring...")}
  if(!is.null(.dry_run))     {warning(".dry_run is not a valid argument for submit_model.bbi_stan_model. Ignoring...")}

  res <- submit_stan_model_cmdstanr(
    .mod,
    .mode = .mode,
    ...,
    .overwrite = .overwrite
  )
  return(res)
}

#' Submit a Stan model via cmdstanr
#'
#' Private implementation function called by `submit_model()` dispatches.
#' @param .mod An S3 object of class `bbi_stan_model`
#' @return The object returned from [cmdstanr::sample()]
#' @keywords internal
submit_stan_model_cmdstanr <- function(.mod,
                                       .mode = c("local"), # TODO: add sge mode for cmdstanr
                                       ...,
                                       .overwrite = NULL) {

  # check against YAML
  check_yaml_in_sync(.mod)
  check_stan_model(.mod, .error = TRUE)

  # check for valid type arg
  .mode <- match.arg(.mode)

  out_dir <- get_output_dir(.mod, .check_exists = FALSE)
  standata_json_path <- build_path_from_model(.mod, STANDATA_JSON_SUFFIX)
  stanargs_path <- build_path_from_model(.mod, STANARGS_SUFFIX)

  if (fs::dir_exists(out_dir)) {
    if (isTRUE(.overwrite)) {
      fs::dir_delete(out_dir)
      fs::dir_create(out_dir)
      if(fs::file_exists(standata_json_path)) { fs::file_delete(standata_json_path) }
      if(fs::file_exists(stanargs_path)) { fs::file_delete(stanargs_path) }
    } else {
      stop(glue("{out_dir} already exists. Pass submit_model(..., .overwrite = TRUE) to delete it and re-run the model."), call. = FALSE)
    }
  } else {
    fs::dir_create(out_dir)
  }

  stanmod <- compile_stanmod(.mod)

  # capture args, check against sample(), then write to stanargs.R
  valid_stanargs <- methods::formalArgs(stanmod$sample)
  stanargs <- parse_stanargs(.mod, valid_stanargs, ...)

  # construct input data set and initial estimates
  standata_list <- build_data(.mod, .out_path = standata_json_path)
  stanargs[["output_dir"]] <- get_output_dir(.mod)
  stanargs[["data"]] <- standata_json_path
  stanargs[["init"]] <- import_stan_init(.mod, .standata = standata_list)
  rm(standata_list) # once we've passed this to import_stan_init() we don't need it in memory

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
