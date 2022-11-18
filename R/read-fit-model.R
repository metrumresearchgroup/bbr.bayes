#' Reads a fitted model object from disk
#'
#' For some a types of models, an object representing the "fitted" model is
#' persisted on disk when the model is run. This function will load that object
#' into memory and return it.
#'
#' @return
#'   * **NONMEM Bayes**: Returns a \pkg{posterior} draws object.
#'
#'   * **Stan**: Returns a `cmdstanr` fit object of class `"CmdStanMCMC"`. See the
#'     `?cmdstanr::CmdStanMCMC` docs for methods and information on this object.
#'     _Note: currently [model_summary]`.bbi_stan_model()` calls this under the
#'     hood because it contains methods to summarize model outputs and no
#'     similar methods exist yet in `bbr` for Stan._
#'
#' @param .mod a `.bbi_{.model_type}_model` object
#' @param format The draws object format to return for bbi_nmbayes_model
#'   objects.
#' @param ... arguments passed through to methods.
#' @export
read_fit_model <- function(.mod, ...) {
  UseMethod("read_fit_model")
}

#' @describeIn read_fit_model Takes a file path that will be passed to
#'   [bbr::read_model()]. The loaded model is then passed directly to the relevant
#'   `read_fit_model()` dispatch.
#' @export
read_fit_model.character <- function(.mod, ...) {
  checkmate::assert_string(.mod)
  read_fit_model(read_model(.mod))
}

#' @describeIn read_fit_model Returns a \pkg{posterior} draws object.
#' @export
read_fit_model.bbi_nmbayes_model <- function(.mod,
                                             format = c("array", "df", "matrix", "list", "rvars"),
                                             ...) {
  # TODO: Define custom object that, like CmdStanMCMC, has info about the run
  # but doesn't bring in the draws until the user explicitly asks for them.
  nmbayes_draws(.mod, format = format)
}

#' @describeIn read_fit_model Returns a `cmdstanr::CmdStanMCMC` object.
#' @export
read_fit_model.bbi_stan_model <- function(.mod, ...) {
  res <- readRDS(build_path_from_model(.mod, STAN_MODEL_FIT_RDS))
  # FIXME: This hopefully temporary kludge makes it possible to read the fit
  # from a different location than the one used for the original run (e.g.,
  # different user or machine). This is necessary because the CmdStanRun object
  # points to absolute paths and doesn't have a public way to re-root them.
  #
  # https://github.com/metrumresearchgroup/bbr.bayes/issues/3
  files <- res$output_files(include_failed = TRUE)
  if (!identical(files, res$runset$.__enclos_env__$private$output_files_)) {
    stop("bug: output_files() result unexpectedly differs from private value")
  }
  res$runset$.__enclos_env__$private$output_files_ <- file.path(
    build_path_from_model(.mod, STAN_OUTDIR_SUFFIX),
    basename(files))

  return(res)
}
