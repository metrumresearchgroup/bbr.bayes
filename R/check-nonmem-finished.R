#' Check if a NONMEM Bayes model has finished running
#'
#' This function implements the [bbr::check_nonmem_finished()] method for NONMEM
#' Bayes models. To decide if a model is finished, it runs
#' [bbr::check_nonmem_finished()] on each of the chain submodels.
#'
#' As with regular NONMEM models, the model is _not_ considered finished if the
#' output directory does not exist.
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
    return(FALSE)
  }

  subs <- purrr::map(get_chain_dirs(.mod), read_model)
  return(all(purrr::map_lgl(subs, function(m) check_nonmem_finished(m, ...))))
}
