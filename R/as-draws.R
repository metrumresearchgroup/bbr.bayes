
#' Extract \pkg{posterior} draws from a bbr.bayes model
#'
#' bbr.bayes models define methods for [posterior::as_draws()] that enable
#' constructing a draws object directly from a model object. In addition to
#' `as_draws()`, format-specific functions, such as [posterior::as_draws_df()]
#' and [posterior::as_draws_rvars()], are also supported.
#'
#' It is possible to get the same values via [read_fit_model()], though the
#' details depend on the model type.
#'
#'  * `bbi_nmbayes_model`: `as_draws()` and friends behaves the same as calling
#'    [read_fit_model()].
#'
#'  * `bbi_stan_model`: `as_draws()` and friends are a shortcut for calling the
#'     same methods on the `CmdStanMCMC` object returned by [read_fit_model()]
#'     or for using the `draws` method of the `CmdStanMCMC` object.
#'
#' @param x Model object.
#' @param ... Additional arguments used by specific methods.
#' @return A \pkg{posterior} draws object.
#' @importFrom posterior as_draws
#' @name bbr_as_draws
NULL

#' @rdname bbr_as_draws
#' @export
as_draws.bbi_nmbayes_model <- function(x, ...) {
  nmbayes_draws(x, ...)
}

#' @rdname bbr_as_draws
#' @export
as_draws.bbi_stan_model <- function(x, ...) {
  as_draws(read_fit_model.bbi_stan_model(x), ...)
}

### Methods for draws_nmbayes class

# The draws_nmbayes class methods wrap posterior's as_draws_* methods to ensure
# that the "nmbayes" attribute is preserved. Without this, only the array and
# matrix methods preserve the attributes.

restore_draws_nmbayes <- function(res, attr_orig) {
  if (!is.null(attr_orig)) {
    attr(res, "nmbayes") <- attr_orig
    if (!inherits(res, "draws_nmbayes")) {
      class(res) <- c("draws_nmbayes", class(res))
    }
  }
  return(res)
}

#' @importFrom posterior as_draws_df
#' @export
as_draws_df.draws_nmbayes <- function(x, ...) {
  nmbayes_attr <- attr(x, "nmbayes")
  attr(x, "nmbayes") <- NULL
  return(restore_draws_nmbayes(NextMethod(), nmbayes_attr))
}

#' @importFrom posterior as_draws_list
#' @export
as_draws_list.draws_nmbayes <- function(x, ...) {
  nmbayes_attr <- attr(x, "nmbayes")
  attr(x, "nmbayes") <- NULL
  return(restore_draws_nmbayes(NextMethod(), nmbayes_attr))
}

#' @importFrom posterior as_draws_array
#' @export
as_draws_array.draws_nmbayes <- function(x, ...) {
  nmbayes_attr <- attr(x, "nmbayes")
  attr(x, "nmbayes") <- NULL
  return(restore_draws_nmbayes(NextMethod(), nmbayes_attr))
}

#' @importFrom posterior as_draws_matrix
#' @export
as_draws_matrix.draws_nmbayes <- function(x, ...) {
  nmbayes_attr <- attr(x, "nmbayes")
  attr(x, "nmbayes") <- NULL
  return(restore_draws_nmbayes(NextMethod(), nmbayes_attr))
}

#' @importFrom posterior as_draws_rvars
#' @export
as_draws_rvars.draws_nmbayes <- function(x, ...) {
  nmbayes_attr <- attr(x, "nmbayes")
  attr(x, "nmbayes") <- NULL
  return(restore_draws_nmbayes(NextMethod(), nmbayes_attr))
}
