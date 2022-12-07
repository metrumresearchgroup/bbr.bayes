
#' Read posteriors draws from NONMEM Bayes model
#'
#' Extract MCMC samples from ext files of chain sub-models.
#'
#' @param .mod A `bbi_nmbayes_model`.
#' @param format A format supported by \pkg{posterior} via an
#'   `as_draws_{format}` method.
#' @return A draws object.
#' @noRd
nmbayes_draws <- function(.mod,
                          format = c("array", "df", "matrix", "list", "rvars")) {
  checkmate::assert_class(.mod, NMBAYES_MOD_CLASS)
  draws_fn <- select_draws_fn(match.arg(format))

  exts <- get_chain_files(.mod, ".ext")
  nchains <- length(exts)
  # TODO: Consider how to handle other information, such as warmup samples or
  # termination status (row -1000000007).
  draws <- vector(mode = "list", length = nchains)
  for (chain in seq_len(nchains)) {
    d <- fread_chain_file(exts[chain])
    # Note: Both the warmup and post-warmup samples have the expected count
    # without ITERATION=0. It's unclear what this value issue.
    draws[[chain]] <- dplyr::filter(d, .data$ITERATION > 0) %>%
      dplyr::select(-"ITERATION") %>%
      dplyr::rename_with(rename_nm_as_rvar, .cols = dplyr::everything())
  }

  return(draws_fn(draws))
}

select_draws_fn <- function(format) {
  switch(format,
         "array" = posterior::as_draws_array,
         "df" = posterior::as_draws_df,
         "matrix" = posterior::as_draws_matrix,
         "list" = posterior::as_draws_list,
         "rvars" = posterior::as_draws_rvars,
         stop("Unknown posterior format: ", format, call. = FALSE))
}

