
#' Compare Stan models
#'
#' Diff files between `bbi_stan_model` (or `bbi_stan_gq_model`) objects.
#'
#' @param .mod,.mod2 Show how `.mod2` has changed compared `.mod`. If `mod2` is
#'   `NULL`, compare `.mod` to the model returned by `get_based_on(.mod)`.
#' @param .file Which files to compare:
#'
#'   * model: `<run>.stan`
#'
#'   * standata: `<run>-standata.R`
#'
#'   * stanargs: `<run>-stanargs.R`
#'
#'   * init: `<run>-init.R` (not valid if either `.mod` or `.mod2` is a
#'     `bbi_stan_gq_model` object)
#'
#'   * fitted-params: `<run>-fitted-params.R` (only valid if both `.mod` and
#'     `.mod2` are `bbi_stan_gq_model` objects)
#'
#' @inheritParams bbr::model_diff
#' @seealso [bbr::model_diff()]
#' @export
model_diff.bbi_stan_model <- function(
  .mod,
  .mod2 = NULL,
  .file = c("model", "standata", "stanargs", "init", "fitted-params"),
  ...,
  .viewer = FALSE
) {
  .file <- match.arg(.file)

  .mod2 <- model_diff_get_comp(.mod, .mod2)

  is_gq <- c(inherits(.mod, STAN_GQ_MOD_CLASS),
             inherits(.mod2, STAN_GQ_MOD_CLASS))

  if (identical(.file, "init") && any(is_gq)) {
    message("stan_gq models do not have init files")
    return(invisible(NULL))
  }
  if (identical(.file, "fitted-params") && !all(is_gq)) {
    message("Only stan_gq models have fitted-params files")
    return(invisible(NULL))
  }

  diff_args <- purrr::map(list(.mod, .mod2), function(.m) {
    if (.file == "model") {
      return(build_path_from_model(.m, ".stan"))
    } else {
      return(build_path_from_model(.m, glue("-{.file}.R")))
    }
  })

  diff_args[[".viewer"]] <- .viewer
  do.call(model_diff_impl, diff_args)
}
