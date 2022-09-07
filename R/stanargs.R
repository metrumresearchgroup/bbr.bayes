#' Set arguments to be passed to cmdstanr$sample()
#'
#' @inheritParams bbr::submit_model
#' @param .stanargs args passed through to [cmdstanr::sample()].
#' @param .clear If `FALSE`, the default, add any new args and only overwrite
#'   existing args that are passed in through `.stanargs`. If `TRUE`, overwrite
#'   _all_ attached args so that only `.stanargs` will be attached.
#'
#' @importFrom styler style_file
#' @export
set_stanargs <- function(.mod, .stanargs, .clear = FALSE) {

  # load existing args
  .args <- if (isTRUE(.clear)) {
    list()
  } else {
    get_stanargs(.mod)
  }

  # check passed args
  checkmate::assert_list(.stanargs, names = "named")
  if (any(names(.stanargs) %in% STAN_RESERVED_ARGS)) {
    stop(paste(
      "Cannot add any of the following args to model because they are parsed internally from the model object:",
      paste(STAN_RESERVED_ARGS, collapse = ', ')
    ))
  }

  # add or overwrite args
  # TODO: do we need to do any evaluation of variables or anything here?
  for (.n in names(.stanargs)) {
    .args[[.n]] <- .stanargs[[.n]]
  }

  # reorder list and write to disk
  stanargs_path <- build_path_from_model(.mod, STANARGS_SUFFIX)
  if (length(.args) > 1) {
    .args <- .args[order(names(.args))]
  }
  dput(.args, stanargs_path)
  capture.output(styler::style_file(stanargs_path))

  return(invisible(.mod))
}

#' Return the current stanargs attached to a model
#' @inheritParams set_stanargs
#' @export
get_stanargs <- function(.mod) {
  stanargs_path <- build_path_from_model(.mod, STANARGS_SUFFIX)
  .args <- dget(stanargs_path)
  checkmate::assert_list(.args, names = "named")

  class(.args) <- c("bbr_stanargs", class(.args))
  return (.args)
}

#' @export
print.bbr_stanargs <- function(x) {
  # TODO: Make this more robust to different possible values
  purrr::imap_chr(x, ~ {
    glue::glue("{cli::col_blue(.y)} = {.x}")
  }) %>%
    rlang::set_names("*") %>%
    cli::cli_bullets()
}
