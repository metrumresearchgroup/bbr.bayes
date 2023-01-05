
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
    build_path_from_model("-standata.R") %>%
    file_edit()
}

#' @describeIn open_stan_file Open `<run>-init.R`.
#' @export
open_staninit_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  .mod %>%
    build_path_from_model("-init.R") %>%
    file_edit()
}
