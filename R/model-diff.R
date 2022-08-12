
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
