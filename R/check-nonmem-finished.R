#' Check if a NONMEM Bayes model has finished running
#'
#' This function implements the [bbr::check_nonmem_finished()] method for NONMEM
#' Bayes models. To decide if a model is finished, it runs
#' [bbr::check_nonmem_finished()] on each of the chain submodels.
#'
#' Note that, like the `bbi_nonmem_model` method, this method is intended to be
#' called on models that have been submitted. It happens to also return `TRUE`
#' for un-submitted models, but that should be considered undefined behavior.
#'
#' @param .mod A `bbi_nmbayes_model` object.
#' @param ... Arguments passed to method call on each chain submodel.
#' @return `TRUE` if all of the chain models have finished, and `FALSE`
#'   otherwise.
#' @name nmbayes_check_nonmem_finished
#' @export
check_nonmem_finished.bbi_nmbayes_model <- function(.mod, ...) {
  outdir <- get_output_dir(.mod, .check_exists = FALSE)
  if (!fs::dir_exists(outdir)) {
    # check_nonmem_finished.bbi_nonmem_model() takes this as an indication that
    # submission failed right away. For nmbayes model the top-level output
    # directory is created synchronously even when `.wait = FALSE`. So it is
    # likely a model without an output directory just wasn't submitted, but
    # return `TRUE` for consistency.
    return(TRUE)
  }

  subs <- purrr::map(get_chain_dirs(.mod), read_model)
  return(all(purrr::map_lgl(subs, function(m) check_nonmem_finished(m, ...))))
}
