
#' TODO
#'
#' @param mod A bbi_nmbayes_model object.
#' @param mod_mrgsolve TODO
#' @param data TODO
#' @param ext TODO
#' @param join_col TODO
#' @param y_col TODO
#' @param point_est TODO
#' @param ci_level TODO
#' @param resid_var TODO
#' @param n_post TODO
#' @param log_dv TODO
#' @param epred TODO
#' @param ipred TODO
#' @param ipred_path TODO
#' @param ewres_npde TODO
#'
#' @return TODO
#' @export
run_sims <- function(mod,
                     mod_mrgsolve,
                     data = NULL,
                     ext = NULL,
                     join_col = "NUM",
                     y_col = "Y",  # TODO: name?
                     point_fn = stats::median,
                     probs = c(0.025, 0.975),
                     resid_var = TRUE,
                     n_post = 1000,
                     log_dv = FALSE,
                     epred = TRUE,
                     ipred = TRUE,
                     ipred_path = NULL,
                     ewres_npde = FALSE) {
  checkmate::assert_class(mod, NMBAYES_MOD_CLASS)
  checkmate::assert_class(mod_mrgsolve, "mrgmod")
  checkmate::assert_string(join_col)
  checkmate::assert_string(y_col)
  # TODO: collapse ipred_path into ipred and decide to write path by checking if
  # ipred is character?
  checkmate::assert_string(ipred_path, null.ok = TRUE)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2)
  checkmate::assert_int(n_post)

  if (isTRUE(ewres_npde) && !requireNamespace("npde", quietly = TRUE)) {
    stop("`npde = TRUE` requires npde package to be installed.")
  }

  if (isTRUE(ewres_npde) && !isTRUE(epred)) {
    stop("`ewres_npde = TRUE` depends on `epred = TRUE`.")
  }

  # TODO: other guards?
  #
  # - join col present
  # - mrgsolve model checks?

  if (is.null(data)) {
    withr::with_options(list(bbr.verbose = FALSE), {
      data <- bbr::nm_data(mod)
    })
  }

  # TODO: extract to exposed join function? (gh-50)
  data_names <- names(data)
  tab <- get_chain_files(mod, "tab", check_exists = "all") %>%
    purrr::map(fread_chain_file) %>%
    dplyr::bind_rows(.id = "chain") %>%
    dplyr::select(all_of(join_col) | -any_of(data_names))

  data_res <- dplyr::left_join(tab, data, by = join_col)
  rm(data)

  if (!("EVID" %in% data_names)) {
    data_res$EVID <- 0
  }

  # TODO: restrict ext and iph fread() calls with `select`.

  # TODO: Explain why not using draws object for ext and iph.
  if (is.null(ext)) {
    ext <- get_chain_files(mod, "ext", check_exists = "all") %>%
      purrr::map(fread_chain_file) %>%
      dplyr::bind_rows(.id = "chain") %>%
      dplyr::filter(.data$ITERATION > 0) %>%
      dplyr::mutate(draw = dplyr::row_number())
  }

  # TODO: If this is all this is, just leave to caller?
  if (!isTRUE(resid_var)) {
    mod_mrgsolve <- mrgsolve::zero_re(mod_mrgsolve)
  }

  if (nrow(ext) > n_post) {
    ext <- ext[sort(sample(nrow(ext), n_post)), ]
  }

  if (isTRUE(epred)) {
    epred_res <- sim_epred(mod_mrgsolve,
                           ext, data_res,
                           join_col, y_col)
    epred_sum <- summarise_epred(
      epred_res, join_col, y_col, point_fn, probs, log_dv)
    data_res <- dplyr::select(data_res, -"EPRED") %>%
      dplyr::left_join(epred_sum, by = join_col)
  }

  if (isTRUE(ipred)) {
    ipred_res <- sim_ipred(mod, mod_mrgsolve,
                           ext, data_res,
                           join_col, y_col)

    if (!is.null(ipred_path)) {
      readr::write_csv(ipred_res, ipred_path)
    }

    ipred_sum <- summarise_ipred(
      ipred_res, join_col, y_col, point_fn, probs, log_dv)
    rm(ipred_res)
    data_res <- dplyr::select(data_res, -any_of("IPRED")) %>%
      dplyr::left_join(ipred_sum, by = join_col)
  }

  if (isTRUE(ewres_npde)) {
    en_res <- sim_ewres_npde(data_res, epred_res, join_col)
    data_res <- dplyr::select(data_res, -c("EWRES", "NPDE")) %>%
      dplyr::left_join(en_res, by = join_col)
  }

  dplyr::select(data_res, all_of(join_col) | -any_of(data_names))
}

sim_epred <- function(mod_mrgsolve, ext, data, join_col, y_col) {
  theta_cols <- grep("^THETA[0-9]+$", colnames(ext))
  # TODO: restore (conditional?) `future` use
  res <- purrr::map(seq_len(nrow(ext)), function(n) {
    ext_row <- ext[n, ]
    theta <- ext_row[theta_cols]
    mod_mrgsolve %>%
      mrgsolve::data_set(data) %>%
      mrgsolve::param(theta, .strict = TRUE) %>%
      mrgsolve::omat(mrgsolve::as_bmat(ext_row, "OMEGA")) %>%
      mrgsolve::smat(mrgsolve::as_bmat(ext_row, "SIGMA")) %>%
      mrgsolve::mrgsim_df(obsonly = TRUE, carry_out = join_col) %>%
      dplyr::select(all_of(join_col), DV_sim = all_of(y_col)) %>%
      dplyr::mutate(sample = n)
  })

  return(tibble::as_tibble(dplyr::bind_rows(res)))
}

# TODO: maybe rework to share repeated code with summarise_ipred()
summarise_epred <- function(simdf, join_col, y_col, point_fn, probs, log_dv) {
  res <- dplyr::group_by(simdf, .data[[join_col]]) %>%
    dplyr::summarise(
      EPRED = point_fn(.data$DV_sim),
      EPRED_lo = stats::quantile(.data$DV_sim, probs[1]),
      EPRED_hi = stats::quantile(.data$DV_sim, probs[2]))

  # TODO: separate helper?
  if (isTRUE(log_dv)) {
    res <- dplyr::mutate(
      res,
      LNEPRED = .data$EPRED,
      LNEPRED_lo = .data$EPRED_lo,
      LNEPRED_hi = .data$EPRED_hi,
      EPRED = exp(.data$EPRED),
      EPRED_lo = exp(.data$EPRED_lo),
      EPRED_hi = exp(.data$EPRED_hi))
  }

  return(res)
}

sim_ipred <- function(mod, mod_mrgsolve, ext, data, join_col, y_col) {
  iph_files <- get_chain_files(mod, "iph")
  if (!length(iph_files)) {
    stop("`ipred = TRUE` requires iph files")
  }

  ipar <- purrr::map(iph_files, fread_chain_file) %>%
    dplyr::bind_rows(.id = "chain") %>%
    dplyr::filter(.data$ITERATION > 0) %>%
    dplyr::select("chain", "ITERATION", "ID", starts_with("ETA(")) %>%
    dplyr::left_join(
      dplyr::select(ext, -starts_with("OMEGA(")),
      by = c("chain", "ITERATION")) %>%
    dplyr::rename_with(~ gsub("\\(([0-9]+)\\)$", "\\1", .x),
                       starts_with("ETA("))

  # TODO: restore (conditional?) `future` use
  res <- purrr::map(seq_len(nrow(ext)), function(n) {
    ext_row <- ext[n, ]
    ipar_n <- dplyr::filter(ipar, .data$draw == ext_row$draw)
    mod_mrgsolve %>%
      mrgsolve::zero_re("omega") %>%
      mrgsolve::data_set(data) %>%
      mrgsolve::idata_set(ipar_n) %>%
      mrgsolve::smat(mrgsolve::as_bmat(ext_row, "SIGMA")) %>%
      mrgsolve::mrgsim_df(obsonly = TRUE, carry_out = join_col) %>%
      dplyr::select(all_of(join_col), DV_sim = all_of(y_col)) %>%
      dplyr::mutate(sample = n)
  })

  return(tibble::as_tibble(dplyr::bind_rows(res)))
}

summarise_ipred <- function(simdf, join_col, y_col, point_fn, probs, log_dv) {
  res <- dplyr::group_by(simdf, .data[[join_col]]) %>%
    dplyr::summarise(
      IPRED = point_fn(.data$DV_sim),
      IPRED_lo = stats::quantile(.data$DV_sim, probs[1]),
      IPRED_hi = stats::quantile(.data$DV_sim, probs[2]))

  if (isTRUE(log_dv)) {
    res <- dplyr::mutate(
      res,
      LNIPRED = .data$IPRED,
      LNIPRED_lo = .data$IPRED_lo,
      LNIPRED_hi = .data$IPRED_hi,
      IPRED = exp(.data$IPRED),
      IPRED_lo = exp(.data$IPRED_lo),
      IPRED_hi = exp(.data$IPRED_hi))
  }

  return(res)
}

sim_ewres_npde <- function(data, epred_res, join_col) {
  df_obs <- dplyr::filter(data, .data$EVID == 0) %>%
    dplyr::select("ID", "TIME", "DV", all_of(join_col))
  df_sim <- dplyr::left_join(epred_res, df_obs, by = join_col) %>%
    dplyr::select("ID", "TIME", DV = "DV_sim")

  # TODO: Using tempfiles is workaround for error mentioned in Bayes expo. Look
  # into.
  tdir <- withr::local_tempdir("bbr.bayes-run-sims-")
  file_df_obs <- file.path(tdir, "df_obs.txt")
  file_df_sim <- file.path(tdir, "df_sim.txt")
  readr::write_delim(df_obs, file_df_obs)
  readr::write_delim(df_sim, file_df_sim)
  # TODO: do we need to relay any args here?

  # TODO: failing (see commented out test)
  #   Error in chol.default(x) :
  #     the leading minor of order 2 is not positive definite
  #   Error in decorr.chol(varsim) : object 'ymat' not found
  out <- npde::autonpde(namobs = file_df_obs,
                        namsim = file_df_sim,
                        iid = "ID", ix = "TIME", iy = "DV",
                        boolsave = FALSE)
  out@results@res %>%
    dplyr::bind_cols(df_obs[, join_col]) %>%
    dplyr::select(all_of(join_col), EWRES = "ydobs", NPDE = "npde")
}
