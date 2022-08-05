
#' @describeIn model_summary _Not fully implemented;_ calls [read_fit_model()]
#'   instead. See "Details" section.
#' @export
model_summary.bbi_stan_model <- function(
  .mod,
  .bbi_args = NULL,
  ...,
  .dry_run = FALSE
) {
  warning(paste(
    "model_summary.bbi_stan_model() is not fully implemented.",
    glue("A `cmdstanr::CmdStanMCMC` object will be returned for {.mod[[ABS_MOD_PATH]]}"),
    "Consider calling `read_fit_model()` directly instead.",
    sep = "\n"
  ), call. = FALSE)
  read_fit_model(.mod)
}
