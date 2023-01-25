
#' Open Stan model file in a text editor
#'
#' These light wrappers around [bbr::build_path_from_model()] enable quickly
#' jumping to a model file in RStudio or any other editor supported by
#' [utils::file.edit()].
#'
#' @param .mod a `bbi_stan_model`
#'
#' @name open_stan_file
NULL

#' @describeIn open_stan_file Open `<run>.stan`.
#' @export
open_stanmod_file <- bbr::open_model_file

#' @describeIn open_stan_file Open `<run>-standata.R`.
#' @export
open_standata_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  .mod %>%
    build_path_from_model(STANDATA_R_SUFFIX) %>%
    file_edit()
}

#' @describeIn open_stan_file Open `<run>-init.R` (not relevant for standalone
#'   generated quantities).
#' @export
open_staninit_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  if (inherits(.mod, "bbi_stan_gq_model")) {
    stop("*-init.R file not applicable to stan_gq models")
  }
  .mod %>%
    build_path_from_model(STANINIT_SUFFIX) %>%
    file_edit()
}

#' @describeIn open_stan_file Open `<run>-fitted-params.R` (only relevant for
#'   standalone generated quantities).
#' @export
open_stan_fitted_params_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  if (!inherits(.mod, "bbi_stan_gq_model")) {
    stop("*-fitted-params.R file only applicable to stan_gq models")
  }
  .mod %>%
    build_path_from_model(STAN_FITTED_PARAMS_SUFFIX) %>%
    file_edit()
}
