
#' Read posteriors draws from NONMEM Bayes model
#'
#' Extract MCMC samples from files of chain sub-models.
#' @noRd
nmbayes_draws <- function(.mod,
                          format = c("array", "df", "matrix", "list", "rvars"),
                          include_iph = TRUE) {
  checkmate::assert_class(.mod, NMBAYES_MOD_CLASS)
  draws_fn <- select_draws_fn(match.arg(format))

  chain_dirs <- get_chain_dirs(.mod)
  exts <- get_chain_files(.mod, ".ext",
                          chain_dirs = chain_dirs,
                          check_exists = "all")
  nchains <- length(exts)

  iphs <- character(0)
  if (isTRUE(include_iph)) {
    iphs <- get_chain_files(.mod, ".iph",
                            chain_dirs = chain_dirs,
                            check_exists = "all_or_none")
    include_iph <- length(iphs) == nchains
  }

  # TODO: Consider how to handle other information, such as warmup samples or
  # termination status (row -1000000007).
  #
  # TODO: Support selecting a subset of variables with argument like
  # cmdstanr::read_cmdstan_csv's.
  draws_dfs <- vector(mode = "list", length = nchains)
  for (chain in seq_len(nchains)) {
    # Note: Both the warmup and post-warmup samples have the expected count
    # without ITERATION=0. It's unclear what this value issue.
    d <- fread_draws(exts[chain]) %>%
      dplyr::select(-"ITERATION")

    if (include_iph) {
      iph_res <- fread_draws(iphs[chain]) %>%
        # The ext data already has an MCMCOBJ column.
        dplyr::rename(MCMCOBJ_IPH = "MCMCOBJ") %>%
        reshape_iph()
      d <- dplyr::bind_cols(d, iph_res, .name_repair = "check_unique")
    }

    draws_dfs[[chain]] <- d
  }

  return(draws_fn(draws_dfs))
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

#' Read draws from chain file and do common processing.
#' @param file An from from *.ext and *.iph file.
#' @noRd
fread_draws <- function(file) {
  fread_chain_file(file) %>%
    dplyr::filter(.data$ITERATION > 0) %>%
    dplyr::rename_with(rename_nm_as_rvar, .cols = dplyr::everything())
}

#' Reshape chain's iph data for conversion to draws object
#'
#' The data in an *.iph file spreads iterations over multiple rows, one for each
#' ID and SUBPOP pair. For conversion to a draws object, this function reshapes
#' the data so that there is a single row per iteration. It does so by encoding
#' 1-based indices for the SUBPOP and ID into the parameter names:
#'
#'      * p    -> p[SP,ID]
#'      * p[M] -> p[M,SP,ID]
#'
#' @param data Data frame with iph data to be reshaped. The column names must
#'   already be processed by `rename_nm_as_rvar()`.
#' @return A data frame suitable to be converted to a draws object.
#' @noRd
#' @importFrom tidyr all_of
reshape_iph <- function(data) {
  param_names <- setdiff(colnames(data), IPH_NONPARAM_NAMES)
  d_long <- tidyr::pivot_longer(data, -all_of(IPH_NONPARAM_NAMES),
                                names_to = "name",
                                values_to = "value") %>%
    dplyr::mutate(
      id = as.integer(factor(.data$ID)),
      sp = as.integer(factor(.data$SUBPOP))) %>%
    # Reorder rows so that, when widened, the parameter columns are grouped by
    # the name (e.g., "ETA[5,1,1]", "ETA[5,1,2]") rather than cycling (e.g.,
    # "ETA[5,1,1]", "MCMCOBJ_IPH[1,1]", ...).
    dplyr::arrange(match(.data$name, param_names), .data$sp, .data$id) %>%
    dplyr::mutate(
      name = paste0(
        ifelse(stringr::str_ends(.data$name, stringr::fixed("]")),
               paste0(stringr::str_sub(.data$name, end = -2L), ","),
               paste0(.data$name, "[")),
        .data$sp, ",", .data$id, "]"))

  dplyr::select(d_long,
                -all_of(IPH_ID_NAMES),
                -c("sp", "id")) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "value") %>%
    dplyr::select(-"ITERATION")
}
