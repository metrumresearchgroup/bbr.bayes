
# Under RStudio, there is a distinction between a plain file.edit() and
# utils::file.edit(). RStudio overrides file.edit() so that it opens up a tab.
# utils::file.edit(), on the other hand, pops up a window that the user must
# save before returning to RStudio.
file_edit <- if (exists("file.edit", envir = globalenv())) {
  get("file.edit", envir = globalenv())
} else {
  utils::file.edit
}

open_stan_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  .mod %>%
    build_path_from_model(".stan") %>%
    file_edit()
}

open_standata_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  .mod %>%
    build_path_from_model("-standata.R") %>%
    file_edit()
}

open_init_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  .mod %>%
    build_path_from_model("-init.R") %>%
    file_edit()
}
