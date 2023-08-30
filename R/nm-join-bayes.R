#' Join input data to Bayesian model output summaries
#'
#' @description
#'
#' `nm_join_bayes()` and `nm_join_bayes_quick()` TODO
#'
#' @details
#'
#' HELP: (details)
#'
#' @param mod A `bbi_nmbayes_model` object.
#' @param mod_mrgsolve An mrgsolve model object. HELP: (fill in more details
#'   about requirements for this model)
#' @param data A NONMEM dataset for `mod` to use instead of the default one
#'   extracted from `mod` with [bbr::nm_data()]. This data frame must include a
#'   `join_col` column.
#' @param join_col Column used to join `data` to a data frame constructed by
#'   combining values from the `bbr-bayes-join.tab` file in each chain
#'   subdirectory.
#' @param y_col The name of the dependent variable in `mod_mrgsolve`.
#' @param point_fn Function used to calculate point estimates of table values
#'   across chains and of simulated EPRED and IPRED values.
#' @param probs A two-item vector of lower and upper probabilities to pass to
#'   [stats::quantile()] to calculate the bounds of the simulated EPRED and
#'   IPRED values.
#' @param resid_var Whether to include residual variability in simulations.
#' @param n_post Randomly select this number of posterior draws to use as input
#'   to the simulation.
#' @param log_dv Whether the DV was log-transformed.
#' @param epred Simulate EPRED values, including the point estimate as `EPRED`
#'   and the bounds as `EPRED_lo` and `EPRED_hi`.
#' @param ipred Simulate IPRED values, including the point estimate as `IPRED`
#'   and the bounds as `IPRED_lo` and `IPRED_hi`.
#' @param ipred_path Write the IPRED simulation result (as comma-separated
#'   values) to this path. This is useful if you need access to the full results
#'   (e.g., for LOO calculations), not just the summary returned by this
#'   function.
#' @param ewres_npde Whether to replace EWRES and NPDE values obtained from
#'   table with ones generated with \pkg{npde}.
#' @param npde_decorr_method Pass this value to `decorr.method` of
#'   [npde::autonpde()].
#' @param min_batch_size To simulate EPRED and IPRED values, posterior samples
#'   are split into batches and sent as items to \pkg{future} `*apply`
#'   functions. The number of batches is chosen so that each batch has at least
#'   the number of samples specified by this argument.
#'
#'   **Note:** You may be able to tune this value to get better performance for
#'   a particular case (considering factors such as the model, targeted future
#'   backend, and machine). However, the results across runs with the same
#'   initial seed will differ if you change this value. To avoid the results
#'   varying across different machines and future backends, do not dynamically
#'   set this value based on the number of available workers.
#'
#' @return A data frame. The base data frame is the result combining the
#'   `bbr-bayes-join.tab` values for each chain, collapsing across `join_col`
#'   with `point_fn`, and then joining `data`. `EPRED`, `IPRED`, `EWRES`, and
#'   `NPDE` values are replaced with a simulated estimate, if requested by the
#'   corresponding argument.
#' @seealso [bbr::nm_join()]
#' @name nm_join_bayes
NULL

#' @rdname nm_join_bayes
#' @export
nm_join_bayes_quick <- function(mod,
                                data = NULL,
                                join_col = "NUM",
                                point_fn = stats::median) {
  nm_join_bayes(mod = mod,
                mod_mrgsolve = NULL,
                data = data,
                join_col = join_col,
                point_fn = point_fn,
                epred = FALSE,
                ipred = FALSE,
                ewres_npde = FALSE)
}

#' @rdname nm_join_bayes
#' @export
nm_join_bayes <- function(mod,
                          mod_mrgsolve,
                          data = NULL,
                          join_col = "NUM",
                          y_col = "Y",
                          point_fn = stats::median,
                          probs = c(0.025, 0.975),
                          resid_var = TRUE,
                          n_post = 1000,
                          log_dv = FALSE,
                          epred = TRUE,
                          ipred = TRUE,
                          ipred_path = NULL,
                          ewres_npde = FALSE,
                          npde_decorr_method = c("cholesky", "inverse", "polar"),
                          min_batch_size = 200) {
  quick <- !(epred || ipred || ewres_npde)

  checkmate::assert_class(mod, NMBAYES_MOD_CLASS)
  checkmate::assert_class(mod_mrgsolve, "mrgmod", null.ok = quick)
  checkmate::assert_data_frame(data, null.ok = TRUE)
  checkmate::assert_string(join_col)
  checkmate::assert_string(y_col)
  checkmate::assert_string(ipred_path, null.ok = TRUE)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2)
  checkmate::assert_int(n_post)
  npde_decorr_method <- match.arg(npde_decorr_method)
  checkmate::assert_int(min_batch_size)

  if (!quick) {
    if (!requireNamespace("mrgsolve", quietly = TRUE)) {
      stop("run_sims() requires mrgsolve package.")
    }

    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop("nm_join_bayes() requires future.apply package.")
    }
  }

  if (isTRUE(ewres_npde)) {
    if (!isTRUE(epred)) {
      stop("`ewres_npde = TRUE` depends on `epred = TRUE`.")
    }

    if (!requireNamespace("npde", quietly = TRUE)) {
      stop("`npde = TRUE` requires npde package.")
    }
  }

  # HELP: mrgsolve model checks?

  if (!isTRUE(resid_var)) {
    mod_mrgsolve <- mrgsolve::zero_re(mod_mrgsolve, "sigma")
  }

  res <- prep_nm_join_data(mod, data, join_col, point_fn)
  if (quick) {
    return(res)
  }
  rm(data)

  exts <- sample_exts(mod, n_post, min_batch_size)
  pbar <- sim_pbar_maybe(exts, epred, ipred)

  if (isTRUE(epred)) {
    epred_res <- sim_epred(mod_mrgsolve,
                           exts, res,
                           join_col, y_col,
                           pbar)
    epred_sum <- summarise_pred(
      "EPRED", epred_res, join_col, point_fn, probs, log_dv)
    res <- dplyr::select(res, -"EPRED") %>%
      dplyr::left_join(epred_sum, by = join_col)
  }

  if (isTRUE(ipred)) {
    ipred_res <- sim_ipred(mod, mod_mrgsolve,
                           exts, res,
                           join_col, y_col,
                           pbar)

    if (!is.null(ipred_path)) {
      readr::write_csv(ipred_res, ipred_path)
    }

    ipred_sum <- summarise_pred(
      "IPRED", ipred_res, join_col, point_fn, probs, log_dv)
    rm(ipred_res)
    res <- dplyr::select(res, -"IPRED") %>%
      dplyr::left_join(ipred_sum, by = join_col)
  }

  if (isTRUE(ewres_npde)) {
    en_res <- sim_ewres_npde(res, epred_res, join_col, npde_decorr_method)
    res <- dplyr::select(res, -c("EWRES", "NPDE")) %>%
      dplyr::left_join(en_res, by = join_col)
  }

  return(res)
}

prep_nm_join_data <- function(mod, data, join_col, point_fn) {
  if (is.null(data)) {
    withr::with_options(list(bbr.verbose = FALSE), {
      data <- bbr::nm_data(mod)
    })
  }

  data_cols <- names(data)

  if (!join_col %in% data_cols) {
    stop("`join_col` (", join_col, ") not found in data columns.")
  }

  sim_cols <- c(
    "PRED", "RES", "WRES",
    "EPRED", "EPRED_lo", "EPRED_hi",
    "LNEPRED", "LNEPRED_lo", "LNEPRED_hi",
    "IPRED", "IPRED_lo", "IPRED_hi",
    "LNIPRED", "LNIPRED_lo", "LNIPRED_hi",
    "NPDE", "EWRES"
  )

  if (length(intersect(sim_cols, data_cols))) {
    stop("Some data names collide with bbr-bayes-join.tab names: ",
         paste(intersect(sim_cols, data_cols), collapse = ", "))
  }

  if (!"EVID" %in% data_cols) {
    data$EVID <- 0
  }

  tab_files <- chain_paths_impl(mod,
                                name = "bbr-bayes-join",
                                extension = "tab",
                                check_exists = "all_or_none")
  if (!length(tab_files)) {
    stop("Chain models do not have bbr-bayes-join.tab files\n",
         "This is unexpected unless you called `submit_model()`\n",
         "with `.run_sims_col = NULL`.")
  }

  tab <- purrr::map(tab_files, fread_chain_file) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-"DV")

  if (!join_col %in% names(tab)) {
    stop("`join_col` (", join_col, ") is not in bbr-bayes-join.tab\n",
         "See `?bbr.bayes::nmbayes_submit_model`.")
  }

  tab_sum <- dplyr::group_by(tab, .data[[join_col]]) %>%
    dplyr::summarise(dplyr::across(everything(), .fns = point_fn))

  dplyr::left_join(tab_sum, data, by = join_col) %>%
    dplyr::select(any_of(data_cols), everything())
}

#' Randomly select ext samples
#'
#' @param mod A `bbi_nmbayes_model` object.
#' @param n_post Number of posterior samples to select.
#' @param min_batch_size Split the samples into a list of data frames so that
#'   each element has at least this number of samples.
#'
#' @return List of data frames with ext samples, split for sending into `future`
#'   map functions.
#' @noRd
sample_exts <- function(mod, n_post, min_batch_size) {
  # Note: Downstream mrgsolve wants a data frame in the same form as the ext
  # data files. Read these directly rather than using as_draws_df() to avoid
  # needing to do extra work to get back to this same place.
  ext <- chain_paths_impl(mod, extension = "ext", check_exists = "all") %>%
    purrr::map(fread_chain_file) %>%
    dplyr::bind_rows(.id = "chain") %>%
    dplyr::filter(.data$ITERATION > 0) %>%
    dplyr::mutate(draw = dplyr::row_number())

  if (nrow(ext) > n_post) {
    ext <- ext[sort(sample(nrow(ext), n_post)), ]
  }

  nrows <- nrow(ext)
  nsplits <- max(nrows %/% min_batch_size, 1)
  purrr::map(parallel::splitIndices(nrows, nsplits), function(i) ext[i, ])
}

sim_epred <- function(mod_mrgsolve, exts, data, join_col, y_col, pbar) {
  theta_cols <- grep("^THETA[0-9]+$", colnames(exts[[1]]))
  mod_sim <- mrgsolve::data_set(mod_mrgsolve, data)
  res <- future.apply::future_lapply(
    exts,
    function(ext) {
      `%>%` <- magrittr::`%>%` # Make pipe available in other sessions.
      purrr::map(seq_len(nrow(ext)), function(n) {
        pbar("EPRED")
        ext_row <- ext[n, ]
        theta <- ext_row[theta_cols]
        mrgsolve::param(mod_sim, theta, .strict = TRUE) %>%
          mrgsolve::omat(mrgsolve::as_bmat(ext_row, "OMEGA")) %>%
          mrgsolve::smat(mrgsolve::as_bmat(ext_row, "SIGMA")) %>%
          mrgsolve::mrgsim_df(obsonly = TRUE, carry_out = join_col) %>%
          dplyr::select(all_of(join_col), DV_sim = all_of(y_col)) %>%
          dplyr::mutate(sample = n)
      })
    },
    future.seed = TRUE,
    future.globals = FALSE)

  return(tibble::as_tibble(dplyr::bind_rows(res)))
}

sim_ipred <- function(mod, mod_mrgsolve, exts, data, join_col, y_col, pbar) {
  # Note: Read these directly rather than using as_draws_df() to avoid
  # unnecessary reshaping. See ext note above.
  iph_files <- chain_paths_impl(mod,
                                extension = "iph",
                                check_exists = "all_or_none")
  if (!length(iph_files)) {
    stop("`ipred = TRUE` requires iph files")
  }

  ipar_full <- purrr::map(iph_files, iph_reader(iph_files[1])) %>%
    dplyr::bind_rows(.id = "chain") %>%
    dplyr::filter(.data$ITERATION > 0)
  ipars <- purrr::map(exts, function(ext) {
    dplyr::left_join(dplyr::select(ext, -starts_with("OMEGA(")),
                     ipar_full,
                     by = c("chain", "ITERATION")) %>%
      dplyr::rename_with(function(x) gsub("\\(([0-9]+)\\)$", "\\1", x),
                         starts_with("ETA("))
  })
  rm(ipar_full)

  mod_sim <- mrgsolve::zero_re(mod_mrgsolve, "omega") %>%
    mrgsolve::data_set(data)

  res <- future.apply::future_Map(
    function(ext, ipar) {
      `%>%` <- magrittr::`%>%`
      purrr::map(seq_len(nrow(ext)), function(n) {
        pbar("IPRED")
        ext_row <- ext[n, ]
        ipar_n <- dplyr::filter(ipar, .data$draw == ext_row$draw)
        mrgsolve::idata_set(mod_sim, ipar_n) %>%
          mrgsolve::smat(mrgsolve::as_bmat(ext_row, "SIGMA")) %>%
          mrgsolve::mrgsim_df(obsonly = TRUE, carry_out = join_col) %>%
          dplyr::select(all_of(join_col), DV_sim = all_of(y_col)) %>%
          dplyr::mutate(sample = n)
      })
    },
    exts, ipars,
    future.seed = TRUE,
    future.globals = FALSE)

  return(tibble::as_tibble(dplyr::bind_rows(res)))
}

iph_reader <- function(iph_file) {
  iph_cols <- fread_peek_at_columns(iph_file)
  eta_cols <- grep("^ETA\\(", iph_cols, value = TRUE)
  if (!length(eta_cols)) {
    stop("iph file unexpectedly does not have `ETA` columns: ", iph_file)
  }

  fn <- function(f) {
    fread_chain_file(f, select = c("ITERATION", "ID", eta_cols))
  }

  return(fn)
}

summarise_pred <- function(name, simdf, join_col, point_fn, probs, log_dv) {
  name_lo <- paste0(name, "_lo")
  name_hi <- paste0(name, "_hi")

  res <- dplyr::group_by(simdf, .data[[join_col]]) %>%
    dplyr::summarise(
      "{name}" := point_fn(.data$DV_sim),
      "{name_lo}" := stats::quantile(.data$DV_sim, probs[1]),
      "{name_hi}" := stats::quantile(.data$DV_sim, probs[2]))

  if (isTRUE(log_dv)) {
    res <- dplyr::mutate(
      res,
      "LN{name}" := .data[[name]],
      "LN{name_lo}" := .data[[name_lo]],
      "LN{name_hi}" := .data[[name_hi]],
      "{name}" := exp(.data[[name]]),
      "{name_lo}" := exp(.data[[name_lo]]),
      "{name_hi}" := exp(.data[[name_hi]]))
  }

  return(res)
}

sim_ewres_npde <- function(data, epred_res, join_col, decorr_method) {
  df_obs <- dplyr::filter(data, .data$EVID == 0) %>%
    # Note: The downstream autonpde() call depends on the position of the ID,
    # TIME, and DV columns.
    dplyr::select("ID", "TIME", "DV", all_of(join_col))
  df_sim <- dplyr::left_join(epred_res, df_obs, by = join_col) %>%
    dplyr::select("ID", "TIME", DV = "DV_sim")

  # TODO: Using tempfiles is workaround for the error mentioned in Bayes expo.
  # Look into.
  tdir <- withr::local_tempdir("bbr.bayes-run-sims-")
  file_df_obs <- file.path(tdir, "df_obs.txt")
  file_df_sim <- file.path(tdir, "df_sim.txt")
  readr::write_delim(df_obs, file_df_obs)
  readr::write_delim(df_sim, file_df_sim)

  # Use output sink because, even with `verbose = FALSE`, npde prints
  #
  #   Distribution of npde :
  #   [...]
  #   Signif. codes: '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1
  withr::with_output_sink(nullfile(), {
    # TODO: Expose any other autonpde arguments?
    out <- tryCatch(
      npde::autonpde(namobs = file_df_obs, namsim = file_df_sim,
                     # autonpde also accepts a name or index for iid, ix, and
                     # iy, so we could pass "ID", "TIME", and "DV". However, its
                     # "is integer?" checks lead to "NAs introduced by coercion"
                     # warnings, so use integers.
                     iid = 1L, ix = 2L, iy = 3L,
                     calc.npd = TRUE, calc.npde = TRUE,
                     decorr.method = decorr_method,
                     verbose = FALSE,
                     boolsave = FALSE),
      error = identity)
  })
  if (inherits(out, "error")) {
    if (utils::packageVersion("mrgsolve") < "1.2.0") {
      warning("autonpde failure may be related to old mrgsolve version.\n",
              "Try installing version 1.2.0 or later.")
    }
    stop(out)
  }

  dplyr::bind_cols(out@results@res, df_obs[, join_col]) %>%
    dplyr::select(all_of(join_col), EWRES = "ydobs", NPDE = "npde")
}

sim_pbar_maybe <- function(exts, epred, ipred, envir = parent.frame()) {
  if (requireNamespace("progressr", quietly = TRUE)) {
    ndraws <- sum(purrr::map_int(exts, nrow))
    nrep <- isTRUE(epred) + isTRUE(ipred)
    fn <- progressr::progressor(steps = ndraws * nrep, envir = envir)
  } else {
    fn <- function(...) NULL
  }

  return(fn)
}
