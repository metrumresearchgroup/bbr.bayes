
#' Compare Stan models
#'
#' Diff files between `bbi_stan_model` objects.
#'
#' @param .mod,.mod2 Show how `.mod2` has changed compared `.mod`. If `mod2` is
#'   `NULL`, compare `.mod` to the model returned by `get_based_on(.mod)`.
#' @param .file Which files to compare:
#'
#'   * model: `<run>.stan`
#'   * standata: `<run>-standata.R`
#'   * init: `<run>-init.R`
#'   * stanargs: `<run>-stanargs.R`
#'
#' @inheritParams bbr::model_diff
#' @seealso [bbr::model_diff()]
#' @export
model_diff.bbi_stan_model <- function(
  .mod,
  .mod2 = NULL,
  .file = c("model", "standata", "init", "stanargs"),
  ...,
  .viewer = FALSE
) {
  .file <- match.arg(.file)

  .mod2 <- model_diff_get_comp(.mod, .mod2)

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
