#' Join Bayesian model output summaries to input data
#'
#' @description
#'
#' `nm_join_bayes()` and `nm_join_bayes_quick()` both join model output
#' summaries to the input data. Underneath this calls [bbr::nm_join()] on each
#' chain submodel and then combines the results, summarizing the table values
#' across chains.
#'
#' By default `nm_join_bayes()` replaces some table quantities with summaries of
#' **simulated** values. It selects a subset of posterior samples and simulates
#' EPRED and IPRED with specified \pkg{mrgsolve} model. It also feeds the
#' simulated EPRED values to \pkg{npde} to calculate EWRES and NPDE values.
#'
#' `nm_join_bayes_quick()`, on the other hand, avoids the simulation; the
#' reported values are calculated from the table values. **Warning**: these
#' estimates should not be considered as reliable but may be useful in the early
#' stages of model development.
#'
#' @details
#'
#' ## Messages
#'
#' `nm_join_bayes()` and `nm_join_bayes_quick()` display messages from
#' [bbr::nm_join()] about the data and table files being joined for the first
#' chain. Messages for the subsequent chains are omitted to avoid flooding the
#' console with identical output.
#'
#' As with [bbr::nm_join()], you can suppress these messages by setting the
#' `bbr.verbose` option to `FALSE`.
#'
#' ## Parallel processing
#'
#' To enable parallel processing for the EPRED and IPRED simulations, configure
#' \pkg{future} ahead of calling this function via [future::plan()].
#'
#' ## Progress bar
#'
#' The EPRED and IPRED simulations support displaying a progress bar via the
#' \pkg{progressr} package. The simplest way to enable a progress bar is to wrap
#' the `nm_join_bayes()` call inside a [progressr::with_progress()] call.
#'
#' @inheritParams bbr::nm_join
#'
#' @param .mod A `bbi_nmbayes_model` object.
#' @param mod_mrgsolve An mrgsolve model object, potentially updated to be
#'   optimized for simulation from the data set and model (e.g., ODE solver
#'   tolerance). This must capture `y_col`.
#' @param .join_col Use this column to join tables files to the input data.
#' @param .files Paths to table files to pass to [bbr::nm_join()] calls (one per
#'   each chain submodel). By default, all tables specified in the `$TABLE`
#'   blocks of the control stream will be used. Note that, unlike
#'   [bbr::nm_join()], this function accepts only relative paths (which
#'   `nm_join()` interprets as relative to submodel output directory) because
#'   the paths need to be valid for each chain submodel.
#' @param ... Additional arguments passed to [bbr::nm_join()].
#' @param dv_col Pass this data column as the dependent variable when
#'   calculating EWRES and NPDE.
#' @param y_col The name of the dependent variable in `mod_mrgsolve`. This is a
#'   simulated quantity corresponding to `dv_col`.
#' @param point_fn Function used to calculate point estimates of table values
#'   across chains and of simulated EPRED and IPRED values (e.g., mean or
#'   median).
#' @param probs A two-item vector of lower and upper probabilities to pass to
#'   [stats::quantile()] to calculate the bounds of the simulated EPRED and
#'   IPRED values.
#' @param resid_var Whether to include residual variability in simulations.
#' @param n_post Randomly select this number of posterior draws to use as input
#'   to the simulation.
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
#' @param presim_fn Before simulating, apply this function to the data frame
#'   that results from joining the input data and table values. The main purpose
#'   of this argument is to provide a way to do any column renames that are
#'   required for the mrgsolve simulation (e.g., renaming a column to "TIME").
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
#' @return A data frame. The base data frame is the result of combining the
#'   [bbr::nm_join()] results for each chain submodel and collapsing across
#'   chains with `point_fn`. `EPRED`, `IPRED`, `EWRES`, and `NPDE` values are
#'   replaced with a simulated estimate, if requested by the corresponding
#'   argument.
#' @seealso [bbr::nm_join()], [bbr_nmbayes] for a high-level description of how
#'   NONMEM Bayes models are structured in bbr
#' @name nm_join_bayes
NULL

#' @rdname nm_join_bayes
#' @export
nm_join_bayes <- function(.mod,
                          mod_mrgsolve,
                          .join_col = "NUM",
                          .files = NULL,
                          .superset = FALSE,
                          ...,
                          dv_col = "DV",
                          y_col = "Y",
                          point_fn = stats::median,
                          probs = c(0.025, 0.975),
                          resid_var = TRUE,
                          n_post = 1000,
                          epred = TRUE,
                          ipred = TRUE,
                          ipred_path = NULL,
                          ewres_npde = TRUE,
                          npde_decorr_method = c("cholesky", "inverse", "polar"),
                          presim_fn = NULL,
                          min_batch_size = 200) {
  quick <- !(epred || ipred)

  checkmate::assert_class(.mod, NMBAYES_MOD_CLASS)
  checkmate::assert_class(mod_mrgsolve, "mrgmod", null.ok = quick)
  checkmate::assert_string(.join_col)
  checkmate::assert_string(dv_col)
  checkmate::assert_string(y_col)
  checkmate::assert_string(ipred_path, null.ok = TRUE)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2)
  checkmate::assert_int(n_post)
  npde_decorr_method <- match.arg(npde_decorr_method)
  checkmate::assert_int(min_batch_size)

  if (!is.null(.files) && any(fs::is_absolute_path(.files))) {
    stop("`.files` must be relative paths.")
  }

  if (!quick) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop("nm_join_bayes() requires future.apply package.")
    }

    if (!requireNamespace("mrgsolve", quietly = TRUE)) {
      stop("nm_join_bayes() requires mrgsolve package.")
    }
  }

  if (isTRUE(ewres_npde)) {
    if (!isTRUE(epred)) {
      stop("`ewres_npde = TRUE` depends on `epred = TRUE`.")
    }

    if (!requireNamespace("npde", quietly = TRUE)) {
      stop("`ewres_npde = TRUE` requires npde package.")
    }
  }

  join_info <- prepare_join(.mod, .join_col, .files, .superset, point_fn, ...)
  res <- dplyr::left_join(join_info$data, join_info$tab, by = .join_col)
  if (quick) {
    return(res)
  }

  qstr <- paste(
    c("EPRED", "IPRED", "EWRES NPDE")[c(epred, ipred, ewres_npde)],
    collapse = " "
  )
  verbose_msg(paste("Simulating requested quantities:", qstr))

  # A .superset=TRUE result will fail if passed as the data for mrgsolve
  # simulations. Drop the extra rows, and add them back at the end.
  in_super_only <- FALSE
  if (isTRUE(.superset)) {
    n_tab_only <- ncol(join_info$tab) - 1 # -1 for join_col
    in_super_only <- rowSums(is.na(join_info$tab)) == n_tab_only
    if (any(in_super_only)) {
      res <- res[!in_super_only, ]
    }
  }

  if (!is.null(presim_fn)) {
    res <- call_presim(presim_fn, res, .join_col)
  }

  if (isTRUE(ewres_npde) && !dv_col %in% names(res)) {
    msg <- paste(dv_col, "not found in result.")
    if (identical(dv_col, "DV")) {
      msg <- paste0(msg, "\nUse `dv_col` to specify a column remapped to DV.")
    }
    stop(msg)
  }

  exts <- sample_exts(.mod, n_post, min_batch_size)
  pbar <- sim_pbar_maybe(exts, epred, ipred)

  if (!isTRUE(resid_var)) {
    mod_mrgsolve <- mrgsolve::zero_re(mod_mrgsolve, "sigma")
  }
  if (!"EVID" %in% names(res)) {
    res$EVID <- 0
  }

  if (isTRUE(epred)) {
    epred_res <- sim_epred(
      mod_mrgsolve,
      exts, res,
      .join_col, y_col,
      pbar
    )
    epred_sum <- summarize_pred(
      "EPRED", epred_res, .join_col, point_fn, probs
    )
    res <- dplyr::select(res, -any_of(c("EPRED", "EPRED_lo", "EPRED_hi"))) %>%
      dplyr::left_join(epred_sum, by = .join_col)
  }

  if (isTRUE(ipred)) {
    ipred_res <- sim_ipred(
      .mod, mod_mrgsolve,
      exts, res,
      .join_col, y_col,
      pbar
    )

    if (!is.null(ipred_path)) {
      readr::write_csv(ipred_res, ipred_path)
    }

    ipred_sum <- summarize_pred(
      "IPRED", ipred_res, .join_col, point_fn, probs
    )
    rm(ipred_res)
    res <- dplyr::select(res, -any_of(c("IPRED", "IPRED_lo", "IPRED_hi"))) %>%
      dplyr::left_join(ipred_sum, by = .join_col)
  }

  if (isTRUE(ewres_npde)) {
    en_res <- sim_ewres_npde(
      res, epred_res, .join_col, dv_col,
      npde_decorr_method
    )
    res <- dplyr::select(res, -any_of(c("EWRES", "NPDE"))) %>%
      dplyr::left_join(en_res, by = .join_col)
  }

  if (any(in_super_only)) {
    # Intersect with res names because presim_fn may have renamed some data
    # columns.
    res_cols <- names(res)
    data_cols <- intersect(names(join_info$data), res_cols)
    res <- dplyr::left_join(
      join_info$data[, data_cols],
      dplyr::select(res, all_of(.join_col) | -any_of(data_cols)),
      by = .join_col
    )
    # Restore column order because it may be different if presim_fn renamed a
    # column.
    res <- res[, res_cols]
  }

  return(res)
}

#' @rdname nm_join_bayes
#' @export
nm_join_bayes_quick <- function(.mod,
                                .join_col = "NUM",
                                .files = NULL,
                                .superset = FALSE,
                                ...,
                                point_fn = stats::median) {
  nm_join_bayes(
    .mod = .mod,
    mod_mrgsolve = NULL,
    .join_col = .join_col,
    .files = .files,
    .superset = .superset,
    ...,
    point_fn = point_fn,
    epred = FALSE,
    ipred = FALSE,
    ewres_npde = FALSE
  )
}

#' Prepare for joining data to table results from submodels
#'
#' For each submodule, join the data and specified tables with `bbr::nm_join()`,
#' and summarize the table values across chains with the specified point
#' function.
#'
#' This function does *not* do the final join; instead it returns a list with
#' two fields, `data` and `tab` (summarized table values), so that the calling
#' code doesn't need to do extra work to figure out what the data columns are.
#'
#' @noRd
prepare_join <- function(mod, join_col, files, superset, point_fn, ...) {
  join_args <- list(.join_col = join_col, .superset = superset, ...)
  if (!is.null(files)) {
    join_args <- c(join_args, .files = files)
  }

  join <- function(sdir) {
    do.call(bbr::nm_join, c(list(read_model(sdir)), join_args))
  }

  sdirs <- get_chain_dirs(mod)
  nchains <- length(sdirs)
  if (nchains < 2) {
    stop("Valid nmbayes models need more than one chain.")
  }

  # The result of these bbr::nm_join() calls doesn't depend on the RNG, but 1)
  # processx::process$new() moves the seed (tested with processx v3.8.2) and 2)
  # a variable number of calls to that are triggered by bbr::nm_join() depending
  # on whether bbi_exec() has been called yet in the session. Restore seed on
  # exit so that the downstream simulation results don't depend on these
  # details.
  withr::local_preserve_seed()

  dfs <- vector(mode = "list", length = nchains)
  # Do first call separately to honor caller's bbr.verbose setting.
  dfs[[1]] <- join(sdirs[1])
  verbose_msg("\nRepeating join for other chains and combining results.")
  withr::with_options(list(bbr.verbose = FALSE), {
    for (i in 2:nchains) {
      dfs[[i]] <- join(sdirs[i])
    }
  })

  # nm_join_origin records the source of different columns.
  origin <- attr(dfs[[1]], "nm_join_origin")
  if (is.null(origin)) {
    # For older bbr versions, fall back to peeking at the header and making the
    # same transformations as nm_join/nm_data/nm_file.
    #
    # TODO: Remove when minimum bbr version is above 1.7.0.
    smod1_data <- bbr::get_data_path(read_model(get_chain_dirs(mod)[1]))
    data_cols <- toupper(fread_peek_at_columns(smod1_data, skip = join_col))

    dv <- data_cols == "DV"
    if (any(dv)) {
      data_cols[dv] <- "DV.DATA"
    }
  } else {
    data_cols <- origin$data
  }

  data <- dfs[[1]][data_cols]

  chain_col <- ".bbr_bayes_nm_join_chain"
  tab_sum <- dplyr::bind_rows(dfs, .id = chain_col) %>%
    dplyr::group_by(.data[[join_col]]) %>%
    dplyr::summarize(
      dplyr::across(-any_of(c(data_cols, chain_col)), .fns = point_fn)
    )

  if (nrow(data) != nrow(tab_sum)) {
    stop("bug: number of data rows not same as number of table summary rows")
  }

  return(list(data = data, tab = tab_sum))
}

call_presim <- function(fn, x, join_col) {
  nrow_orig <- nrow(x)
  x <- fn(x)

  if (!inherits(x, "data.frame")) {
    stop("presim_fn() must return a data frame")
  }
  if (!identical(nrow_orig, nrow(x))) {
    stop("presim_fn() result had ", nrow(x), " rows but expected ", nrow_orig)
  }
  if (!join_col %in% names(x)) {
    stop("presim_fn() must retain join column in the result.")
  }

  return(x)
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
  # Drop post-hoc ETAs so that ETAs are randomly generated within model.
  data <- data[grep("^ETA?[0-9]+$", colnames(data), invert = TRUE)]
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
    future.globals = FALSE
  )

  return(tibble::as_tibble(dplyr::bind_rows(res)))
}

sim_ipred <- function(mod, mod_mrgsolve, exts, data, join_col, y_col, pbar) {
  # Note: Read these directly rather than using as_draws_df() to avoid
  # unnecessary reshaping. See ext note above.
  iph_files <- chain_paths_impl(mod,
    extension = "iph",
    check_exists = "all_or_none"
  )
  if (!length(iph_files)) {
    stop("`ipred = TRUE` requires iph files")
  }

  ipar_full <- purrr::map(iph_files, iph_reader(iph_files[1])) %>%
    dplyr::bind_rows(.id = "chain") %>%
    dplyr::filter(.data$ITERATION > 0)
  ipars <- purrr::map(exts, function(ext) {
    dplyr::left_join(dplyr::select(ext, -starts_with("OMEGA(")),
      ipar_full,
      by = c("chain", "ITERATION")
    ) %>%
      dplyr::rename_with(
        function(x) gsub("\\(([0-9]+)\\)$", "\\1", x),
        starts_with("ETA(")
      )
  })
  rm(ipar_full)

  # Purge post-hoc ETAs just in case there is logic written into mrgsolve model
  # (see gh-117).
  data <- data[grep("^ETA?[0-9]+$", colnames(data), invert = TRUE)]

  res <- future.apply::future_Map(
    function(ext, ipar) {
      `%>%` <- magrittr::`%>%`
      purrr::map(seq_len(nrow(ext)), function(n) {
        pbar("IPRED")
        ext_row <- ext[n, ]
        ipar_n <- dplyr::filter(ipar, .data$draw == ext_row$draw)
        mrgsolve::mrgsim_df(
          mod_mrgsolve,
          data = data, idata = ipar_n,
          sigma = mrgsolve::as_bmat(ext_row, "SIGMA"),
          obsonly = TRUE, carry_out = join_col,
          etasrc = "idata.all"
        ) %>%
          dplyr::select(all_of(join_col), DV_sim = all_of(y_col)) %>%
          dplyr::mutate(sample = n)
      })
    },
    exts, ipars,
    future.seed = TRUE,
    future.globals = FALSE
  )

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

summarize_pred <- function(name, simdf, join_col, point_fn, probs) {
  name_lo <- paste0(name, "_lo")
  name_hi <- paste0(name, "_hi")
  dplyr::group_by(simdf, .data[[join_col]]) %>%
    dplyr::summarize(
      "{name}" := point_fn(.data$DV_sim),
      "{name_lo}" := stats::quantile(.data$DV_sim, probs[1]),
      "{name_hi}" := stats::quantile(.data$DV_sim, probs[2])
    )
}

sim_ewres_npde <- function(data, epred_res, join_col, dv_col, decorr_method) {
  # TODO: Ideally this would give a message to inform the caller what's going
  # on, but, in my testing, that message doesn't come through until after the
  # function returns, so there may be a progressr interaction.
  df_obs <- dplyr::filter(data, .data$EVID == 0) %>%
    # Note: The downstream autonpde() call depends on the position of the ID,
    # TIME, and DV columns.
    dplyr::select("ID", "TIME", all_of(c(dv_col, join_col)))
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
    out <- tryCatch(
      npde::autonpde(
        namobs = file_df_obs, namsim = file_df_sim,
        # autonpde also accepts a name or index for iid, ix, and iy. Passing
        # names leads to its "is integer?" checks giving "NAs introduced by
        # coercion" warnings, so use integers.
        iid = 1L, ix = 2L, iy = 3L,
        calc.npd = TRUE, calc.npde = TRUE,
        decorr.method = decorr_method,
        verbose = FALSE,
        boolsave = FALSE
      ),
      error = identity
    )
  })
  if (inherits(out, "error")) {
    if (utils::packageVersion("mrgsolve") < "1.2.0") {
      warning(
        "autonpde failure may be related to old mrgsolve version.\n",
        "Try installing version 1.2.0 or later."
      )
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
