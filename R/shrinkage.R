
#' Calculate shrinkage in variance of errors
#'
#' Using the posterior samples, estimate how much the variance of errors is
#' reduced by group-level pooling.
#'
#' The shrinkage calculation here follows Gelman and Pardoe's pooling factor
#' definition, with a few adjustments:
#'
#'   * Keeping with pharmacometrics conventions, the calculation uses standard
#'     deviation rather than variance by default (controlled by `use_sd`
#'     argument).
#'
#'   * As with NONMEM's shrinkage values, the returned values are percentages
#'     rather than fractions.
#'
#'   * If the variance of the errors is supplied, the denominator is calculated
#'     by taking the expectation of those values.
#'
#' @param errors An object from which errors can be extracted.
#'
#'   * For a `bbi_nmbayes_model` object, the `ETA` values are taken as the
#'     errors, and variance for each `ETA` is extracted from the diagonal of the
#'     `OMEGA` matrix. As a special case, if `*.iph` files do not exist, the
#'     shrinkage values are collected from the `*.shk` files and summarized as
#'     medians across chains.
#'
#'   * For a \pkg{posterior} draws object, the errors are extracted for the
#'     parameter name specified by `errors_name`. When passing a draws object,
#'     the variance is calculated from the errors, and there is not support for
#'     also providing separate variance values; to do that, pass rvar objects
#'     instead.
#'
#'   * For a \pkg{posterior} rvar object, the errors and variance are supplied
#'     directly.
#'
#' @param use_sd Whether to calculate shrinkage with standard deviation
#'   (default) instead of variance.
#' @param ... Additional arguments passed on to methods.
#'
#' @return Vector of shrinkage estimates, in the same order as the values
#'   specified by `errors`.
#'
#'   The dimensions of the input errors determines the dimensions of the return
#'   value. For example, errors of the form `X[i,j]`, where `j` is the
#'   group-level index, would lead to a vector of `i` values. And errors
#'   `X[i,j,k]`, where `k` is the group-level index, would lead to a matrix with
#'   `i` rows and `j` columns.
#'
#' @references Andrew Gelman, Iain Pardoe (2006) Bayesian measures of explained
#'   variance and pooling in multilevel (hierarchical) models. *Technometrics*.
#'   48(2), 241--251.
#' @export
shrinkage <- function(errors, ..., use_sd = TRUE) {
  UseMethod("shrinkage")
}

#' @rdname shrinkage
#' @export
shrinkage.bbi_nmbayes_model <- function(errors, ..., use_sd = TRUE) {
  rlang::check_dots_empty()

  mod <- errors
  iph_files <- get_chain_files(mod, ".iph", check_exists = "all_or_none")
  if (length(iph_files)) {
    draws <- posterior::as_draws_rvars(mod)
    eta <- draws[["ETA"]]
    # For ETA[I,J,K], J is SUBPOP value. See reshape_iph().
    if (dim(eta[2]) != 1) {
      stop("shrinkage() does not currently support more than one SUBPOP")
    }
    shrinkage(posterior::drop(eta),
              variance = diag(draws[["OMEGA"]]),
              use_sd = use_sd)
  } else {
    shk_files <- get_chain_files(mod, ".shk", check_exists = "all_or_none")
    if (!length(shk_files)) {
      stop("No *.iph or *.shk files found; cannot calculate shrinkage")
    }

    shk <- purrr::map_dfr(shk_files, fread_chain_file, .id = "chain") %>%
      # From Intro to NM 7: "Type 4=%Eta shrinkage SD version"
      dplyr::filter(.data$TYPE == 4)

    if (!nrow(shk)) {
      stop("Failed to extract TYPE=4 rows from *.shk files:\n",
           paste("  -", shk_files, collapse = "\n"))
    }

    if (dplyr::n_distinct(shk$SUBPOP) != 1) {
      stop("shrinkage() does not currently support more than one SUBPOP")
    }

    dplyr::select(shk, starts_with("ETA")) %>%
      dplyr::summarise(dplyr::across(.fns = stats::median))  %>%
      as.numeric()
  }
}

#' @rdname shrinkage
#' @param errors_name Name of a parameter to extract from the `draws_rvars`
#'   object and use as the errors.
#' @param group_idx A vector of indices specifying which dimension(s) correspond
#'   to groups. Defaults to the last dimension when not specified.
#' @export
shrinkage.draws <- function(errors,
                            errors_name,
                            group_idx = NULL,
                            ...,
                            use_sd = TRUE) {
  rlang::check_dots_empty()
  checkmate::assert_string(errors_name)

  draws <- posterior::as_draws_rvars(errors)
  rv <- draws[[errors_name]]
  if (is.null(rv)) {
    stop(glue("Could not find '{errors_name}' in rvar draws object"))
  }

  shrinkage(rv, group_idx = group_idx, use_sd = use_sd)
}

#' @rdname shrinkage
#' @param variance A \pkg{posterior} rvar object specifying variance of the
#'   errors. If not specified, it is set to the variance across groups in
#'   `errors`. Note that this should be variance, not standard deviation, even
#'   when `use_sd` is `TRUE`.
#' @export
shrinkage.rvar <- function(errors,
                           variance = NULL,
                           group_idx = NULL,
                           ...,
                           use_sd = TRUE) {
  rlang::check_dots_empty()
  checkmate::assert_class(variance, "rvar", null.ok = TRUE)
  checkmate::assert_integerish(group_idx, null.ok = TRUE, lower = 1L)

  # `errors` comes in with at least one dimension. If there is only one, margin
  # is NULL and the only option is to calculate shrinkage over that dimension.
  #
  # If there's more than one dimension (e.g., in the NONMEM context, ETA comes
  # in as two dimensions, where the second is for the individual), we calculate
  # shrinkage over the last dimension unless `group_idx` specifies which
  # dimension(s) to use.
  margin <- NULL
  ndim <- length(dim(errors))

  if (!is.null(group_idx)) {
    if (length(group_idx) >= ndim) {
      stop("`group_idx` must leave at least one margin free")
    } else if (any(group_idx > ndim))  {
      stop(
        sprintf("group_idx values (%s) exceed number of dimensions (%d)",
                deparse(group_idx[group_idx > ndim]),
                ndim))
    }

    margin <- setdiff(seq_len(ndim), group_idx)
  } else if (ndim > 1) {
    margin <- seq_len(ndim - 1)
  }

  # For the numerator, the standard deviation (or variance, if use_sd=FALSE) is
  # calculated on the point estimates obtained by taking the mean of the samples
  # (`pt_dispersion_fn`). If `errors` has one dimension, the result is a vector
  # with the length of the groups. If `errors` has more than one dimension, the
  # result is an array with the same dimensions as `errors`. In either case,
  # we've taken the expectation and are no longer working with an rvar by the
  # time the value is fed to `pt_dispersion_fn`.
  #
  # For the denominator (when `variance` isn't specified), the standard
  # deviation (or variance, if use_sd=FALSE) of group errors values is
  # calculated within each sample (`group_dispersion_fn`). If `errors` has one
  # dimension, the result is an rvar with one dimension and one value. If
  # `errors` has more than one dimension, the result is an rvar with the
  # `group_idx` dimension(s) dropped.
  #
  # Taking expectation of the `group_dispersion_fn` result leads to a value with
  # the same dimensions as the `pt_dispersion_fn` return value.
  if (isTRUE(use_sd)) {
    dispersion_fn <- stats::sd
    rvar_dispersion_fn <- posterior::rvar_sd
  } else {
    dispersion_fn <- stats::var
    rvar_dispersion_fn <- posterior::rvar_var
  }

  if (is.null(margin)) {
    pt_dispersion_fn <- dispersion_fn
    group_dispersion_fn <- rvar_dispersion_fn
  } else {
    pt_dispersion_fn <- function(x) apply(x, margin, dispersion_fn)
    group_dispersion_fn <- function(x) {
      posterior::rvar_apply(x, margin, rvar_dispersion_fn)
    }
  }

  numer <- pt_dispersion_fn(posterior::E(errors))
  if (is.null(variance)) {
    denom <- posterior::E(group_dispersion_fn(errors))
  } else {
    dim_var <- dim(variance)
    dim_want <- dim(numer) %||% length(numer)
    if (!identical(dim_var, dim_want)) {
      stop(sprintf("dim(`variance`) is %s, expected %s",
                   deparse(dim_var), deparse(dim_want)))
    }
    denom <- posterior::E(variance)
    if (isTRUE(use_sd)) {
      denom <- sqrt(denom)
    }
  }

  return((1 - numer / denom) * 100)
}
