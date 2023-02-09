
# TODO: Rework bbr so that this can be handled through normal functions
# (summary_log, add_summary).

#' Create a tibble summarizing results for a set of Stan models
#'
#' @description
#'
#' There are two entry points for generating the summary log:
#'
#'  * `stan_summary_log()`: return a tibble the summarizes the Stan models under
#'     the specified directory
#'
#'  * `stan_add_summary()`: join summary columns to a [bbr::run_log()] tibble.
#'     At least one row of the run log tibble must have a Stan model.
#'
#' @details
#'
#' The information for each row is primarily extracted from a few methods of the
#' \pkg{cmdstanr} fit object: [$metadata][cmdstanr::fit-method-metadata],
#' [$diagnostic_summary()][cmdstanr::fit-method-diagnostic_summary], and
#' [$summary()][cmdstanr::fit-method-summary].
#'
#' @inheritParams bbr::run_log
#' @inheritParams bbr::add_summary
#' @param variables Model variables to summarize with `summary_fns`. This value
#'   is passed through to the `CmdStanFit`
#'   [$summary()][cmdstanr::fit-method-summary] method. To summarize these
#'   variables, their draws will be read in. It is valid to specify a variable
#'   that only exists in a subset of the models.
#'
#'   By default this is restricted to "lp__", but you will likely want to extend
#'   this with a few variables of interest. `$summary()` will use _all_
#'   variables if you pass `NULL`, but it's recommended to keep the set small
#'   for performance reasons and because each variable adds a number of columns
#'   equal to the number of `summary_fns` (or possibly more, if a summary
#'   function returns a named vector). To disable extracting and summarizing the
#'   draws entirely, set `summary_fns` to an empty list.
#'
#' @param summary_fns A list of summary or diagnostic functions passed to
#'   [posterior::summarize_draws()][draws_summary]. The result will be included
#'   as a new column, `{variable}_{name}`, where "name" is determined by the
#'   names of the returned vector or otherwise by the name of the `summary_fns`
#'   item.
#'
#'   With the default value of `NULL`, the following functions are used:
#'   [mean()], [stats::median()], [posterior::quantile2()], [posterior::rhat()],
#'   [posterior::ess_bulk()], and [posterior::ess_tail()]. Pass an empty list to
#'   disable all variable summaries.
#'
#' @return A tibble. For `stan_summary_log()`, the tibble will have a row for
#'   each Stan model collected from under the directory. For
#'   `stan_add_summary()`, the tibble will have the same number of rows as
#'   `.log_df` with additional columns.
#'
#' @seealso <https://mc-stan.org/misc/warnings> for an overview of diagnostics
#'   and warnings
#' @export
stan_summary_log <- function(.base_dir,
                             .recurse = FALSE,
                             .include = NULL,
                             variables = "lp__",
                             summary_fns = NULL) {
  mods <- find_models(.base_dir, .recurse, .include) %>%
    purrr::keep(~ inherits(.x, STAN_MOD_CLASS))
  fits <- purrr::map(mods, try_read_model)
  tibble::tibble(
    absolute_model_path = purrr::map_chr(mods, ABS_MOD_PATH),
    run = purrr::map_chr(mods, get_model_id),
    make_stan_summary_log(fits,
                          summary_fns = summary_fns,
                          variables = variables),
    fit = fits)
}

#' @rdname stan_summary_log
#' @export
stan_add_summary <- function(.log_df,
                             variables = "lp__",
                             summary_fns = NULL) {
  checkmate::assert_names(names(.log_df),
                          must.include = c(ABS_MOD_PATH, YAML_MOD_TYPE))
  idx <- .log_df[[YAML_MOD_TYPE]] %in% c("stan", "stan_gq")
  if (!any(idx)) {
    stop(".log_df does not contain a Stan model")
  }

  mpaths <- .log_df[idx, ][[ABS_MOD_PATH]]
  fits <- purrr::map(mpaths, try_read_model)
  slog <- tibble::tibble(
    !!ABS_MOD_PATH := mpaths,
    make_stan_summary_log(fits,
                          summary_fns = summary_fns,
                          variables = variables))
  dplyr::left_join(.log_df, slog, by = ABS_MOD_PATH)
}

try_read_model <- function(x) {
  tryCatch(read_fit_model(x),
           bbr.bayes_read_fit_error = function(e) NULL)
}

make_stan_summary_log <- function(fits, variables, summary_fns) {
  checkmate::assert_character(variables, null.ok = TRUE)

  if (is.null(summary_fns)) {
    # This is close to the default functions used by
    # posterior::summarise_draws() (v1.2.0), but drops stats::sd() and
    # stats::mad().
    summary_fns <- list(
      mean = base::mean,
      median = stats::median,
      quantile = posterior::quantile2,
      rhat = posterior::rhat,
      ess_bulk = posterior::ess_bulk,
      ess_tail = posterior::ess_tail)
  }

  res <- purrr::map(fits, function(fit) {
    if (is.null(fit)) {
      return(tibble::tibble_row())
    }
    dplyr::bind_cols(extract_metadata(fit),
                     extract_diagnostic_summaries(fit),
                     extract_param_summaries(fit, variables, summary_fns))
  })
  dplyr::bind_rows(res)
}

extract_metadata <- function(fit) {
  metadata <- fit$metadata()
  tibble::tibble_row(
    method = metadata[["method"]],
    stan_version = paste(metadata[["stan_version_major"]],
                         metadata[["stan_version_minor"]],
                         metadata[["stan_version_patch"]],
                         sep = "."),
    nchains = length(metadata$id),
    # CmdStanGQ objects do not have these values.
    iter_warmup = as.integer(metadata[["iter_warmup"]] %||% NA),
    iter_sampling = as.integer(metadata[["iter_sampling"]] %||% NA))
}

extract_diagnostic_summaries <- function(fit) {
  # CmdStanGQ objects do not have diagnostic_summary().
  fn <- fit[["diagnostic_summary"]]
  if (!is.null(fn)) {
    ds <- fn(quiet = TRUE)
    tibble::tibble_row(
      num_divergent = as.integer(sum(ds[["num_divergent"]])),
      num_max_treedepth = as.integer(sum(ds[["num_max_treedepth"]])),
      bad_ebfmi = bad_ebfmi(ds[["ebfmi"]]))
  }
}

#' Flag E-BFMI values
#'
#' cmdstanr's diagnostic_summary() returns an E-BFMI value for each chain; when
#' quiet=FALSE it warns if there is an NA value or if any value is below 0.2.
#' Follow the same criteria here, translating it into a boolean flag.
#'
#' @noRd
bad_ebfmi <- function(ebfmi, threshold = 0.2) {
  any(is.nan(ebfmi)) || any(ebfmi < threshold)
}

extract_param_summaries <- function(fit, variables, summary_fns) {
  if (!length(summary_fns)) {
    return(NULL)
  }

  if (is.null(variables)) {
    # Take variables=NULL as "all variables", following cmdstanr's $draws() and
    # $summary().
    vars <- NULL
  } else if (!is.null(variables)) {
    vars <- intersect(variables, fit$metadata()[["variables"]])
    if (!length(vars)) {
      return(NULL)
    }
  }
  res <- try_summary(fit, c(list(variables = vars), summary_fns))
  if (!is.null(res)) {
    tidyr::pivot_wider(
      res,
      names_from = "variable",
      values_from = -"variable",
      names_glue = "{variable}_{.value}")
  }
}

try_summary <- function(fit, args) {
  tryCatch(
    do.call(fit$summary, args),
    error = function(e) {
      if (all(fs::file_exists(fit$output_files()))) {
        # This is an unexpected error.
        stop(e)
      }
    }
  )
}
