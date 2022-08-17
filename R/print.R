
#' @export
print_model_files.bbi_stan_model <- function(.mod, print_fn) {
  print_fn(build_path_from_model(.mod, STANDATA_R_SUFFIX))
  print_fn(build_path_from_model(.mod, STANINIT_SUFFIX))
  check_stan_model(.mod)
}
