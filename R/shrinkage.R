
#' Calculate shrinkage in variance of errors
#'
#' @description
#'
#' Using the posterior samples, estimate how much the variance of errors is
#' reduced by group-level pooling.
#'
#' For a `bbi_nmbayes_model` object, the `ETA` values are taken as the errors,
#' and variance for each `ETA` is extracted from the diagonal of the `OMEGA`
#' matrix. As a special case, if `*.iph` files do not exist, the shrinkage
#' values are collected from the `*.shk` files.
#'
#' For a \pkg{posterior} draws object, the errors and variance are extracted for
#' the specified parameter names.
#'
#' For a \pkg{posterior} rvar object, the errors and variance are supplied
#' directly.
#'
#' @details
#'
#' The shrinkage calculation here follows Gelman and Pardoe's pooling factor
#' definition, with a few adjustments:
#'
#'   * Keeping with pharmacometrics conventions, the calculation uses standard
#'     deviation rather than variance.
#'
#'   * As with NONMEM's shrinkage values, the returned values are percentages
#'     rather than fractions.
#'
#'   * If the variance of the errors is supplied, the denominator is calculated
#'     by taking the expectation of those values.
#'
#' @param errors An object from which errors can be extracted.
#' @param ... Additional arguments passed on to methods.
#'
#' @return Vector of shrinkage estimates, in the same order as the values
#'   specified by `errors`.
#'
#'   The dimensions of the input errors determines the dimensions of the return
#'   value (dropping any dimensions with only one level). For example, errors of
#'   the form `X[i,j]`, where `j` is the group-level index, would lead to a
#'   vector of `i` values. And errors `X[i,j,k]`, where `k` is the group-level
#'   index, would lead to a matrix with `i` rows and `j` columns.
#'
#' @references Andrew Gelman, Iain Pardoe (2006) Bayesian measures of explained
#'   variance and pooling in multilevel (hierarchical) models. *Technometrics*.
#'   48(2), 241--251.
#' @export
shrinkage <- function(errors, ...) {
  UseMethod("shrinkage")
}

#' @rdname shrinkage
#' @export
shrinkage.bbi_nmbayes_model <- function(errors, ...) {
  mod <- errors
  iph_files <- get_chain_files(mod, ".iph", check_exists = "all_or_none")
  if (length(iph_files)) {
    shrinkage(read_fit_model(mod),
              errors_name = "ETA",
              variance_name = "OMEGA",
              from_diag = TRUE,
              ...)
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

    dplyr::select(shk, tidyr::starts_with("ETA")) %>%
      dplyr::summarise(dplyr::across(.fns = stats::median))  %>%
      as.numeric()
  }
}

#' @rdname shrinkage
#' @param errors_name Name of a parameter to extract from the `draws_rvars`
#'   object and use as the errors.
#' @param variance_name Name of a parameter to extract from the `draws_rvars`
#'   object and use as the variance. The dimensions of this value should
#'   correspond to the dimenions in the errors after summarizing across the
#'   group dimension(s). If no name is provided, the variance across groups is
#'   calculated for the extracted errors.
#' @param from_diag To obtain the variance values, take the diagonal of the
#'   matrix return by `variance_name`.
#' @param group_idx A vector of indices specifying which dimension(s) correspond
#'   to groups. Defaults to the last dimension when not specified.
#' @export
shrinkage.draws <- function(errors,
                            errors_name,
                            variance_name = NULL,
                            from_diag = FALSE,
                            group_idx = NULL,
                            ...) {
  rlang::check_dots_used()
  checkmate::assert_string(errors_name)
  checkmate::assert_string(variance_name, null.ok = TRUE)

  draws <- posterior::as_draws_rvars(errors)
  errs <- posterior::drop(draws[[errors_name]])

  err_var <- NULL
  if (!is.null(variance_name)) {
    err_var <- draws[[variance_name]]
    if (isTRUE(from_diag)) {
      if (!is.matrix(err_var)) {
        stop("Value for `variance_name` must be matrix if from_diag=TRUE")
      }
      err_var <- diag(err_var)
    }
    err_var <- posterior::drop(err_var)
  }

  shrinkage(errs, group_idx = group_idx, variance = err_var)
}

#' @rdname shrinkage
#' @param variance A \pkg{posterior} rvar object specifying variance of the
#'   errors. If not specified, it is set to the variance across groups in
#'   `errors`.
#' @export
shrinkage.rvar <- function(errors, variance = NULL, group_idx = NULL, ...) {
  rlang::check_dots_used()
  checkmate::assert_class(variance, "rvar", null.ok = TRUE)
  checkmate::assert_integerish(group_idx, null.ok = TRUE, lower = 1L)

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

  if (is.null(margin)) {
    pt_var_fn <- stats::sd
    group_var_fn <- posterior::rvar_sd
  } else {
    pt_var_fn <- function(x) apply(x, margin, stats::sd)
    group_var_fn <- function(x) {
      posterior::rvar_apply(x, margin, posterior::rvar_sd)
    }
  }

  numer <- pt_var_fn(posterior::E(errors))
  denom <- if (is.null(variance)) {
    posterior::E(group_var_fn(errors))
  } else {
    dim_var <- dim(variance)
    dim_want <- dim(numer) %||% length(numer)
    if (!identical(dim_var, dim_want)) {
      stop(sprintf("dim(`variance`) is %s, expected %s",
                   deparse(dim_var), deparse(dim_want)))
    }
    sqrt(posterior::E(variance))
  }

  return((1 - numer / denom) * 100)
}
