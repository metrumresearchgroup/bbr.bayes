#############################
# PRIVATE HELPER FUNCTIONS
# for submitting Stan models
#############################

#' Pull in Stan init file
#'
#' Sources the `-init.R` file associated with a model
#' and returns whatever is returned by the the `make_init()`
#' function it contains.
#'
#' @param .mod a `bbi_stan_model` object
#' @param .standata the data list that is returned from [build_data()] for this model
#' @param .stanargs the named list of arguments that will be passed to [cmdstanr::sample()]
#'
#' @keywords internal
import_stan_init <- function(.mod, .standata, .stanargs) {
  seed <- .stanargs$seed %||% stop("stanargs must have seed")
  # check for <run>-init.R file
  staninit_path <- build_path_from_model(.mod, STANINIT_SUFFIX)

  # source and call function
  make_init <- safe_source_function(staninit_path, "make_init")
  withr::with_seed(seed, {
    init_res <- safe_call_sourced(
      .func = make_init,
      .args = list(.data = .standata, .args = .stanargs),
      .file = staninit_path
    )
  })

  # MAYBE FIRST DO SOME CHECKING?
  # that the returned value is actually something cmdstanr::sample() can take...
  # either a function or a list of lists that all have the same keys, etc.?

  if (is.function(init_res)) {
    # We can't expect reproducible results if we pass the user-specified through
    # untouched.
    fn_args <- formals(init_res)
    if (is.null(fn_args)) {
      fn <- function(chain_id) init_res()
    } else if (identical(names(fn_args), "chain_id")) {
      fn <- function(chain_id) init_res(chain_id)
    } else {
      stop("init function must take zero arguments or one, 'chain_id'")
    }

    # cmdstanr v0.1.0 deprecated num_chains. Check that too and then fall back
    # to cmdstanr's default for `chain`.
    nchains <- .stanargs$chains %||% .stanargs$num_chains %||% 4
    withr::with_seed(seed, {
      init_res <- purrr::map(seq_len(nchains), fn)
    })
  }

  return(init_res)
}

import_stan_fitted_params <- function(.mod) {
  path <- build_path_from_model(.mod, STAN_FITTED_PARAMS_SUFFIX)
  make_fitted_params <- safe_source_function(path, "make_fitted_params")
  safe_call_sourced(.func = make_fitted_params,
                    .args = list(.mod = .mod),
                    .file = path)
}

#' Private helper to compile a stan model and save a gitignore that ignores the
#' binary.
#' @noRd
compile_stanmod <- function(.mod) {
  # compile model
  stanmod <- cmdstanr::cmdstan_model(get_model_path(.mod))

  # add to gitignore, if not already present
  gitignore <- file.path(.mod[[ABS_MOD_PATH]], ".gitignore")

  if (!fs::file_exists(gitignore)) {
    readr::write_lines(paste(
      "# ignore model binary",
      get_model_id(.mod), "",
      sep = "\n"
    ), gitignore)
  } else {
    gitignore_lines <- readr::read_lines(gitignore)
    # if either line is missing, append it
    if (!any(grepl(glue("^{get_model_id(.mod)}$"), gitignore_lines))) {
      readr::write_lines(glue("\n\n# ignore model binary\n{get_model_id(.mod)}"), gitignore, append = TRUE)
    }
  }

  return(stanmod)
}

#' Private helper to save cmdstanr model fit object to RDS
#'
#' This intentionally does _not_ collect the posteriors with `$draws()` because
#' those are also saved to disk separately. This _does_ allow a user to reload
#' this object into memory though. (We will likely want a helper function to do
#' that. Maybe that's what `model_summary()` should do?...)
#' @param .stanmod a `cmdstanr` fit object of class `"CmdStanMCMC"`
#' @param .out_path path to save the model to. Within `submit_model()` we pass
#'   `build_path_from_model(.mod, STAN_MODEL_FIT_RDS)` but could be anywhere.
#' @keywords internal
save_fit_stanmod <- function(.stanmod, .out_path) {
  try(.stanmod$sampler_diagnostics(), silent = TRUE)
  try(.stanmod$init(), silent = TRUE)
  saveRDS(.stanmod, file = .out_path)
}


#' Build bbi_config.json for Stan models
#'
#' Contains information, including hashes and configuration,
#' for successfully run models.
#' @param .mod a `bbi_stan_model` object
#' @keywords internal
build_stan_bbi_config <- function(.mod, .write) {

  out_dir <- get_output_dir(.mod)
  data_path <- fs::path_rel(
    build_path_from_model(.mod, STANDATA_JSON_SUFFIX),
    start = out_dir
  )

  stan_config <- rlang::list2(
    "output_dir"          = out_dir,
    !!CONFIG_DATA_PATH   := data_path,
    !!CONFIG_MODEL_MD5   := tools::md5sum(get_model_path(.mod)),
    !!CONFIG_DATA_MD5    := tools::md5sum(build_path_from_model(.mod, STANDATA_JSON_SUFFIX)),
    !!STANCFG_DATA_MD5   := tools::md5sum(build_path_from_model(.mod, STANDATA_R_SUFFIX)),
    !!STANCFG_ARGS_MD5   := tools::md5sum(build_path_from_model(.mod, STANARGS_SUFFIX)),
  )

  if (inherits(.mod, STAN_GQ_MOD_CLASS)) {
    stan_config[[STANCFG_FITTED_PARAMS_MD5]] <- tools::md5sum(
      build_path_from_model(.mod, STAN_FITTED_PARAMS_SUFFIX))
    # There may be multiple gq_parent values; use I() so that value is
    # consistently "boxed".
    stan_config[[STANCFG_GQ_PARENT_MD5]] <- I(get_gq_parent_md5(.mod))
  } else {
    stan_config[[STANCFG_INIT_MD5]] <- tools::md5sum(
      build_path_from_model(.mod, STANINIT_SUFFIX))
  }

  stan_config[["configuration"]] <-  list(
    "cmdstan_version" = cmdstanr::cmdstan_version(),
    "cmdstanr_version" = as.character(utils::packageVersion("cmdstanr")))

  # write to disk
  stan_json <- jsonlite::toJSON(stan_config, pretty = TRUE, auto_unbox = TRUE)
  readr::write_lines(stan_json, file.path(get_output_dir(.mod), "bbi_config.json"))
}

get_gq_parent_md5 <- function(.mod) {
  parent <- get_stan_gq_parent_no_check(.mod)
  if (is.null(parent)) {
    return(NULL)
  }

  parent_configs <- build_config_paths(parent)
  return(tools::md5sum(parent_configs))
}

#' Return paths to bbi_config.json files
#'
#' This custom function exists in favor of mapping over `get_config_path(.mod)`
#' for the following reasons:
#'
#'  * `get_config_path()` errors if the output directory doesn't exist even when
#'     `.check_exists = FALSE` is passed because `get_config_path.bbi_model()`
#'     doesn't relay the `.check_exists` value to its `get_output_dir()` call.
#'
#'     (Issue is present at least up to bbr 1.5.0.)
#'
#'  * The goal of this function is to return the paths, letting the caller deal
#'    with things like missing files (even entire model directories). With
#'    `get_config_path()`, each model needs to be read in.
#'
#' @param mod_paths Absolute paths to the model (i.e. the "absolute_model_path"
#'   value of the models).
#' @return Absolute paths to the corresponding bbi_config.json files.
#' @noRd
build_config_paths <- function(mod_paths) {
  file.path(mod_paths,
            paste0(get_model_id(mod_paths), STAN_OUTDIR_SUFFIX),
            "bbi_config.json")
}
