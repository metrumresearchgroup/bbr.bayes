
#' Summarize a `bbi_nmbayes_model` object
#'
#' There is currently no [bbr::model_summary()] method for summarizing NONMEM
#' Bayes outputs. Call [read_fit_model()] to return a \pkg{posterior} draws
#' object, which has various methods for summarizing the posteriors.
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
  stop(
    "model_summary.bbi_nmbayes_model() is not implemented.\n",
    "Call `read_fit_model()` to get a draws object.",
    call. = FALSE
  )
}

#' Summarize a `bbi_stan_model`
#'
#' There is currently no [bbr::model_summary()] method for summarizing Stan
#' outputs. Call [read_fit_model()] to return the resulting fit object, which
#' has various methods for summarizing the outputs. See the
#' `?cmdstanr::CmdStanMCMC` docs for methods and information on this object.
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
  stop(
    "model_summary.bbi_stan_model() is not implemented.\n",
    "Call `read_fit_model()` to get a `cmdstanr::CmdStanMCMC` object.",
    call. = FALSE
  )
}
