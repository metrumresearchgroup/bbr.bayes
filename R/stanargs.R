#' Set or get arguments to pass when submitting Stan model
#'
#' @description
#'
#' When [submit_model()][stan_submit_model] is called with a Stan model, it
#' collects the arguments from `<run>-stanargs.R` and passes them to
#' `CmdStanModel`s [$sample()][cmdstanr::model-method-sample] (for
#' `bbi_stan_model` objects) or
#' [$generate_quantities()][cmdstanr::model-method-generate-quantities] (for
#' `bbi_stan_gq_model` objects).
#'
#' `set_stanargs()` and `get_stanargs()` provide an interface for specifying and
#' inspecting the arguments attached to a model.
#'
#' @param .mod A Stan model object.
#' @param .stanargs args passed through to `$sample()` or
#'   `$generate_quantities()`.
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
  set_stanargs_impl("sample", .mod, .stanargs, .clear)
}

#' @rdname stanargs
#' @export
set_stanargs.bbi_stan_gq_model <- function(.mod, .stanargs, .clear = FALSE) {
  set_stanargs_impl("generate_quantities", .mod, .stanargs, .clear)
}

set_stanargs_impl <- function(method, .mod, .stanargs, .clear) {
  # load existing args
  .args <- if (isTRUE(.clear)) {
    list()
  } else {
    get_stanargs(.mod)
  }

  # check passed args
  check_reserved_stanargs(.stanargs, method = method)
  check_unknown_stanargs(.stanargs, method = method)

  # add or overwrite args
  # TODO: do we need to do any evaluation of variables or anything here?
  for (.n in names(.stanargs)) {
    .args[[.n]] <- .stanargs[[.n]]
  }

  # reorder list and write to disk
  stanargs_path <- build_path_from_model(.mod, STANARGS_SUFFIX)
  write_stanargs(.args, stanargs_path)

  return(invisible(.mod))
}

write_stanargs <- function(args, file) {
  if (length(args) > 1) {
    args <- args[order(names(args))]
  }
  dput(args, file)
  utils::capture.output(styler::style_file(file))
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

check_reserved_stanargs <- function(.stanargs,
                                    method = c("sample", "generate_quantities")) {
  checkmate::assert_list(.stanargs, names = "named")
  method <- match.arg(method)

  if (identical(method, "sample") && any(names(.stanargs) %in% "init")) {
    # ^ If it's not for the sample method, just let "init" be caught by
    # check_unknown_stanargs.
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

check_unknown_stanargs <- function(args,
                                   method = c("sample", "generate_quantities")) {
  method <- match.arg(method)
  invalid_stanargs <- setdiff(names(args), get_known_params(method))
  if (length(invalid_stanargs) > 0) {
    stop(glue("The following arguments are not accepted by CmdStanModel${method}():\n",
              .trim = FALSE),
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
