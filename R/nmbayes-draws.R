
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
  iph_select <- if (include_iph) get_iph_variables(iphs[1]) else NULL

  # TODO: Consider how to handle other information, such as warmup samples or
  # termination status (row -1000000007).
  #
  # TODO: Support selecting a subset of variables with argument like
  # cmdstanr::read_cmdstan_csv's.
  draws_dfs <- vector(mode = "list", length = nchains)
  iph_meta <- NULL
  for (chain in seq_len(nchains)) {
    # Note: Both the warmup and post-warmup samples have the expected count
    # without ITERATION=0. It's unclear what this value issue.
    d <- fread_draws(exts[chain]) %>%
      dplyr::select(-"ITERATION") %>%
      dplyr::rename_with(rename_nm_as_rvar, .cols = dplyr::everything())

    if (include_iph) {
      iph_res <- fread_draws(iphs[chain], select = iph_select) %>%
        # The ext data already has an MCMCOBJ column.
        dplyr::rename(MCMCOBJ_IPH = "MCMCOBJ") %>%
        reshape_iph()
      d <- dplyr::bind_cols(d, iph_res$draws, .name_repair = "check_unique")
      if (is.null(iph_meta))
        # The iph maps are the same across chains. Only grab it once.
        iph_meta <- list(id_map = iph_res$id_map,
                         subpop_map = iph_res$subpop_map)
    }

    draws_dfs[[chain]] <- d
  }

  draws <- draws_fn(draws_dfs)
  if (!is.null(iph_meta)) {
    attr(draws, "nmbayes") <- list(iph = iph_meta)
  }
  class(draws) <- c("draws_nmbayes", class(draws))

  return(draws)
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

get_iph_variables <- function(file) {
  iph_all_names <- fread_peek_at_columns(file)
  bm_etas <- stringr::str_starts(iph_all_names, stringr::fixed("ETA("))
  if (!any(bm_etas)) {
    stop("iph file unexpectedly missing ETA(N) columns: ", file)
  }
  bm_obj <- iph_all_names == "MCMCOBJ"
  if (!any(bm_obj)) {
    stop("iph file unexpectedly missing MCMCOBJ column: ", file)
  }

  return(c(IPH_NONPARAM_NAMES, iph_all_names[bm_etas | bm_obj]))
}

#' Read draws from chain file and do common processing.
#' @param file An *.ext or *.iph file.
#' @param select Passed through to `data.table::fread()`.
#' @noRd
fread_draws <- function(file, select = NULL) {
  fread_chain_file(file, select = select) %>%
    dplyr::filter(.data$ITERATION > 0)
}

#' Reshape chain's iph data for conversion to draws object
#'
#' The data in an *.iph file spreads iterations over multiple rows, one for each
#' ID and SUBPOP pair. For conversion to a draws object, this function reshapes
#' the data so that there is a single row per iteration. It does so by encoding
#' 1-based indices for the SUBPOP and ID into the parameter names:
#'
#'      * p    -> p[SP,ID]
#'      * p(M) -> p[M,SP,ID]
#'
#' @param data Data frame with iph data to be reshaped.
#' @return A list of three values:
#'
#'   * draws: a data frame suitable to be converted to a draws object.
#'
#'   * id_map: a data frame that maps the index in the parameter name ("index"
#'     column) to the original `ID` value in `data`.
#'
#'   * subpop_map: a data frame that maps the index in the parameter name
#'     ("index" column) to the original `SUBPOP` value in `data`.
#' @noRd
#' @importFrom tidyr all_of
reshape_iph <- function(data) {
  # Note: This function is expensive. Compare timings when changing.
  param_names <- setdiff(colnames(data), IPH_NONPARAM_NAMES)
  d_long <- dplyr::mutate(data,
                          id = as.integer(factor(.data$ID)),
                          sp = as.integer(factor(.data$SUBPOP))) %>%
    tidyr::pivot_longer(-all_of(c(IPH_NONPARAM_NAMES, c("id", "sp"))),
                        names_to = "name",
                        values_to = "value") %>%
    # Reorder rows so that, when widened, the parameter columns are grouped by
    # the name (e.g., "ETA[5,1,1]", "ETA[5,1,2]") rather than cycling (e.g.,
    # "ETA[5,1,1]", "MCMCOBJ_IPH[1,1]", ...).
    dplyr::arrange(match(.data$name, param_names), .data$sp, .data$id)

  id_map <- dplyr::select(d_long, c("index" = "id", "ID")) %>%
    dplyr::distinct()
  subpop_map <- dplyr::select(d_long, c("index" = "sp", "SUBPOP")) %>%
    dplyr::distinct()
  draws <- dplyr::select(d_long,
                         -all_of(IPH_ID_NAMES)) %>%
    tidyr::pivot_wider(names_from = c("name", "sp", "id"),
                       names_glue = "{name}[{sp},{id}]",
                       values_from = "value") %>%
    dplyr::select(-"ITERATION") %>%
    dplyr::rename_with(rename_iph_cols, .cols = dplyr::everything())

  return(list(draws = draws, id_map = id_map, subpop_map = subpop_map))
}

rename_iph_cols <- function(x) {
  # Fix up non-scalar variables from NONMEM that now have names like
  # "ETA(1)[1,1]".
  stringi::stri_replace_last(x,
                             regex = "\\(([0-9]+)\\)\\[",
                             replacement = "[$1,")
}
