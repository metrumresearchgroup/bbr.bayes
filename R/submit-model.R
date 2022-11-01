
### NONMEM Bayes

#' Run Bayes chains
#' 
#' Run multiple chains of a Bayes model after initial estimates have been
#' generated
#'
#' @param .model_dir path to directory containing model
#' @param .run run name
#' @param .bbi_args list of arguments to be passed to `submit_model()`
run_chains <- function(.model_dir, .run, .mode = "sge", .bbi_args) {
  mod <- read_model(file.path(.model_dir, .run))
  ctl <- read_lines(get_model_path(mod))
  
  row_bayes <- str_detect(ctl, "METHOD=BAYES|METHOD=NUTS")
  est_bayes <- ctl[row_bayes]
  est_bayes <- str_replace(est_bayes, "^;", "")
  ctl[row_bayes] <- est_bayes
  
  row_table <- str_detect(ctl, ";\\s*\\$TABLE")
  block_table <- ctl[row_table]
  block_table <- str_replace(block_table, "^;", "")
  ctl[row_table] <- block_table
  
  row_chain <- str_detect(ctl, "METHOD=CHAIN")
  est_chain <- ctl[row_chain]
  n_chain <- as.numeric(str_extract(est_chain, "(?<=NSAMPLE=)[0-9]+"))
  est_chain <- str_replace(est_chain, "NSAMPLE=[0-9]+", "NSAMPLE=0")
  est_chain <- str_replace(est_chain, "FILE=", "FILE=../")
  
  row_data <- str_detect(ctl, "\\$DATA")
  data_record <- ctl[row_data]
  ctl[row_data] <- str_replace(data_record, "\\$DATA\\s+", "$DATA ../")
  
  row_extrasend <- str_detect(ctl, "extrasend")
  ctl[row_extrasend] <- str_replace(ctl[row_extrasend], "extrasend", "../extrasend")
  
  walk(seq_len(n_chain), function(.chain) {
    #cat(.chain, "\n")
    est_chain_i <- str_replace(est_chain, "ISAMPLE=0", glue::glue("ISAMPLE={.chain}"))
    #est_chain_i <- str_replace(est_chain_i, "SEED=[0-9]+", glue::glue("SEED={.chain}"))
    est_bayes_i <- str_replace(est_bayes, "SEED=[0-9]+", glue::glue("SEED={.chain}"))
    #cat(est_chain_i, "\n")
    ctl_i <- ctl
    ctl_i[row_chain] <- est_chain_i
    ctl_i[row_bayes] <- est_bayes_i
    write_lines(ctl_i, file.path(
      .model_dir,
      glue::glue("{.run}/{.run}_{.chain}.ctl"))
    )
    
    mod <- new_model(
      #glue::glue("./{.run}/{.run}.{.chain}.yaml"),
      file.path(.model_dir, .run, glue::glue("{.run}_{.chain}")),
      .description = glue::glue("Chain {.chain}"),
      .overwrite = TRUE
    )
    
    proc <- submit_model(
      mod,
      #.directory = file.path(.model_dir, .run),
      .bbi_args = .bbi_args,
      .mode = .mode,
      .config_path = file.path(.model_dir, "bbi.yaml")
      #.dry_run = FALSE
    )
  })
}

### Stan

#' Submit model based on a `bbi_stan_model` object
#'
#' The model is executed via [cmdstanr::sample()].
#'
#' @inheritParams bbr::submit_model
#'
#' @export
submit_model.bbi_stan_model <- function(
  .mod,
  .bbi_args = NULL,
  .mode = c("local"),
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
  check_reserved_stanargs(stanargs)

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

  check_unknown_stanargs(stanargs)

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
