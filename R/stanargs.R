#' Set or get arguments to pass when submitting Stan model
#'
#' @description
#'
#' When [submit_model()] is called with a Stan model, it collects the arguments
#' from `<run>-stanargs.R` and passes them to `CmdStanModel`s
#' [$sample()][cmdstanr::model-method-sample].
#'
#' `set_stanargs()` and `get_stanargs()` provide an interface for specifying and
#' inspecting the arguments attached to a model.
#'
#' @param .mod A `bbi_stan_model` object.
#' @param .stanargs args passed through to `$sample()`.
#' @param .clear If `FALSE`, the default, add any new args and only overwrite
#'   existing args that are passed in through `.stanargs`. If `TRUE`, overwrite
#'   _all_ attached args so that only `.stanargs` will be attached.
#' @name stanargs
#' @export
set_stanargs <- function(.mod, .stanargs, .clear = FALSE) {
  UseMethod("set_stanargs")
}

#' @rdname stanargs
#' @export
set_stanargs.bbi_stan_model <- function(.mod, .stanargs, .clear = FALSE) {
  # load existing args
  .args <- if (isTRUE(.clear)) {
    list()
  } else {
    get_stanargs(.mod)
  }

  # check passed args
  check_reserved_stanargs(.stanargs)
  check_unknown_stanargs(.stanargs)

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
  utils::capture.output(styler::style_file(stanargs_path))

  return(invisible(.mod))
}

#' @rdname stanargs
#' @export
get_stanargs <- function(.mod) {
  stanargs_path <- build_path_from_model(.mod, STANARGS_SUFFIX)
  .args <- dget(stanargs_path)
  checkmate::assert_list(.args, names = "named")

  class(.args) <- c("bbr_stanargs", class(.args))
  return (.args)
}

#' @export
print.bbr_stanargs <- function(x, ...) {
  # TODO: Make this more robust to different possible values
  purrr::imap_chr(x, ~ {
    glue::glue("{cli::col_blue(.y)} = {.x}")
  }) %>%
    rlang::set_names("*") %>%
    cli::cli_bullets()
}

#' @keywords internal
check_reserved_stanargs <- function(.stanargs) {
  checkmate::assert_list(.stanargs, names = "named")
  if (any(names(.stanargs) %in% "init")) {
    stop(paste(
      "Cannot pass `init` via stanargs. Please add initial values in the `{.mod}-init.R` file.",
      "You can use `open_init_file(.mod)` to easily open this file for editing."
    ))
  }
  if (any(names(.stanargs) %in% STAN_RESERVED_ARGS)) {
    stop(paste(
      "Cannot add any of the following args to model because they are parsed internally from the model object:",
      paste(STAN_RESERVED_ARGS, collapse = ', ')
    ))
  }
}

check_unknown_stanargs <- function(args) {
  invalid_stanargs <- setdiff(names(args), get_known_params("sample"))
  if (length(invalid_stanargs) > 0) {
    stop("The following arguments are not accepted by cmdstanr::sample():\n",
         paste(invalid_stanargs, collapse = ", "),
         call. = FALSE)
  }
}

known_params <- new.env(parent = emptyenv())

#' Return valid parameters for a CmdStanModel method
#'
#' @param method Method of interest (e.g., "sample").
#' @return Character vector of parameters for `method`.
#' @noRd
get_known_params <- function(method) {
  if (exists(method, known_params)) {
    return(get(method, envir = known_params))
  }

  mfile <- system.file("model", "stan", "fxa", "fxa.stan",
                       mustWork = TRUE, package = "bbr.bayes")
  mod <- cmdstanr::cmdstan_model(mfile, compile = FALSE)
  fn <- mod[[method]]
  if (is.null(fn)) {
    stop("Unknown CmdStanModel method: ", method)
  }
  params <- methods::formalArgs(fn)
  assign(method, params, envir = known_params)

  return(params)
}
