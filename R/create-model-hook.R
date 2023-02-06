
#' @export
create_model_hook.bbi_stan_model <- function(.mod, ...) {
  fs::dir_create(.mod[[ABS_MOD_PATH]])
  check_stan_model(.mod, .syntax = FALSE, .error = FALSE)
  scaffold_missing_stan_files(.mod)
}
