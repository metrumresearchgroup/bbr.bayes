
# Under RStudio, there is a distinction between a plain file.edit() and
# utils::file.edit(). RStudio overrides file.edit() so that it opens up a tab.
# utils::file.edit(), on the other hand, pops up a window that the user must
# save before returning to RStudio.
file_edit <- function(...) {
  fn <- if (exists("file.edit", envir = globalenv())) {
    get("file.edit", envir = globalenv())
  } else {
    utils::file.edit
  }
  fn(...)
}

#' Open Stan model file in a text editor
#'
#' These light wrappers around [bbr::build_path_from_model()] enable quickly
#' jumping to a model file in RStudio or any other editor supported by
#' [utils::file.edit()].
#'
#' @param .mod a `bbi_stan_model`
#'
#' @name open_model_file
NULL

#' @describeIn open_model_file Open `<run>.stan`.
#' @export
open_stan_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  .mod %>%
    build_path_from_model(".stan") %>%
    file_edit()
}

#' @describeIn open_model_file Open `<run>-standata.R`.
#' @export
open_standata_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  .mod %>%
    build_path_from_model("-standata.R") %>%
    file_edit()
}

#' @describeIn open_model_file Open `<run>-init.R`.
#' @export
open_init_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  .mod %>%
    build_path_from_model("-init.R") %>%
    file_edit()
}
