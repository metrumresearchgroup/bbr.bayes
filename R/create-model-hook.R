
#' @export
create_model_hook.bbi_stan_model <- function(.mod, ...) {
  check_stan_model(.mod)
  scaffold_missing_stan_files(.mod)
}
