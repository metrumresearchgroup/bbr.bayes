
#' Summarize a `bbi_nmbayes_model` object
#'
#' This method will eventually provide a tailored summary for NONMEM Bayes
#' output; for now, it returns a model summary object for each underying
#' sampling run.
#'
#' @inheritParams bbr::model_summary
#'
#' @export
model_summary.bbi_nmbayes_model <- function(
  .mod,
  .bbi_args = NULL,
  ...,
  .dry_run = FALSE
) {
  # TODO: Implement. See issue 27 for design discussion.
  warning(
    "model_summary.bbi_nmbayes_model() is not implemented.\n",
    "Returning list of summaries for underlying sampling models.",
    call. = FALSE)
  purrr::map(get_chain_dirs(.mod), read_model) %>%
    bbr::model_summaries(.bbi_args = .bbi_args, ..., .dry_run = .dry_run) %>%
    purrr::map("bbi_summary")
}

#' Summarize a `bbi_stan_model`
#'
#' There is currently no functionality for summarizing Stan outputs with `bbr`.
#' Calling `model_summary()` on a `bbi_stan_model` instead calls
#' [read_fit_model()] and return the resulting fit object, which has various
#' methods for summarizing the outputs. See the `?cmdstanr::CmdStanMCMC` docs
#' for methods and information on this object. A warning will also be printed to
#' notify the user of this.
#'
#' @inheritParams bbr::model_summary
#'
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
