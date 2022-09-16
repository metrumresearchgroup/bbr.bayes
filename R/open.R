

##################################
# helpers for opening bbr.bayes
# files in Rstudio
##################################

open_stan_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  .mod %>%
    build_path_from_model(".stan") %>%
    file.edit()
}

open_standata_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  .mod %>%
    build_path_from_model("-standata.R") %>%
    file.edit()
}

open_init_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_stan_model")
  .mod %>%
    build_path_from_model("-init.R") %>%
    file.edit()
}
