#' Create model for generating initial estimates for an nmbayes model
#'
#' @param .mod A `bbi_nmbayes_model` object.
#' @noRd
nmbayes_init <- function(.mod) {
  ctl_file <- get_model_path(.mod)
  outdir <- get_output_dir(.mod, .check_exists = FALSE)
  # Keep same extension so that bbi treats the data path the same way.
  ctl_file_init <- file.path(
    outdir,
    fs::path_ext_set("init", fs::path_ext(ctl_file))
  )

  ctl <- nmrec::read_ctl(ctl_file)
  adjust_data_path(ctl)

  is_table <- purrr::map_lgl(ctl$records, function(r) r$name == "table")
  if (any(is_table)) {
    ctl$records[is_table] <- NULL
  }

  is_other_est <- purrr::map_lgl(ctl$records, function(r) {
    if (r$name == "estimation") {
      return(!identical(get_est_method(r), "chain"))
    }
    return(FALSE)
  })
  if (any(is_other_est)) {
    ctl$records[is_other_est] <- NULL
  }

  # Remaining EST record should be chain.
  ests <- nmrec::select_records(ctl, "est")
  if (length(ests) != 1) {
    stop("Must be one method=chain estimation record in ", ctl_file,
         "\nGot ", length(ests))
  }
  est_chain <- ests[[1]]
  set_chain_file(est_chain)

  fs::dir_create(outdir)
  nmrec::write_ctl(ctl, ctl_file_init)

  # Upstream code is responsible removing existing output if .overwrite=TRUE, so
  # an unconditional .overwrite=FALSE is sufficient.
  return(new_model(file.path(outdir, "init"), .overwrite = FALSE))
}

#' Run Bayes chains
#'
#' Run multiple chains of a Bayes model after initial estimates have been
#' generated
#'
#' @param .mod A `bbi_nmbayes_model` object.
#' @param ... Arguments passed to [bbr::submit_model()].
#' @noRd
run_chains <- function(.mod, ...) {
  checkmate::assert_class(.mod, NMBAYES_MOD_CLASS)

  ctl_file <- get_model_path(.mod)
  ctl <- nmrec::read_ctl(ctl_file)
  adjust_data_path(ctl)

  ests <- nmrec::select_records(ctl, "est")
  ests_bayes <- purrr::keep(ests, function(r) {
    return(get_est_method(r) %in% c("bayes", "nuts"))
  })

  if (length(ests_bayes) != 1) {
    stop("Must be one method={BAYES,NUTS} estimation record in ", ctl_file,
         "\nGot ", length(ests_bayes))
  }
  est_bayes <- ests_bayes[[1]]

  ests_chain <- purrr::keep(ests, function(r) {
    return(identical(get_est_method(r), "chain"))
  })
  if (length(ests_chain) != 1) {
    stop("Must be one chain estimation record in ", ctl_file,
         "\nGot ", length(est_chain))
  }
  est_chain <- ests_chain[[1]]

  niter_opt <- nmrec::get_record_option(est_chain, "niter")
  if (is.null(niter_opt)) {
    stop("Chain estimation must specify niter option:\n",
         format(est_chain))
  }
  n_chain <- as.numeric(niter_opt$value)
  niter_opt$value <- 0

  set_chain_file(est_chain)

  .run <- get_model_id(.mod)
  outdir <- get_output_dir(.mod)
  based_on <- file.path("..", .run)
  # Use same extension for chain submodels so that bbi treats the data path the
  # same way.
  ext <- fs::path_ext(ctl_file)
  mods <- purrr::map(seq_len(n_chain), function(.chain) {
    isample_opt <- nmrec::get_record_option(est_chain, "isample")
    if (is.null(isample_opt)) {
      stop("Chain estimation must specify isample option:\n",
           format(est_chain))
    }
    isample_opt$value <- .chain

    seed_opt <- nmrec::get_record_option(est_bayes, "seed")
    if (is.null(seed_opt)) {
      stop("Bayes/NUTS estimation must specify seed option:\n",
           format(est_bayes))
    }

    # TODO: Should we consider the user-specified seed and just increment it to
    # be chain specific, like cmdstan does?
    seed_opt$value <- .chain
    nmrec::write_ctl(
      ctl,
      file.path(outdir, glue("{.run}-{.chain}.{ext}")))
    chain_mod <- new_model(
      file.path(outdir, glue("{.run}-{.chain}")),
      .description = glue("Chain {.chain}"),
      .based_on = based_on,
      # Upstream code is responsible removing existing output if
      # .overwrite=TRUE, so an unconditional .overwrite=FALSE is sufficient.
      .overwrite = FALSE
    )
    # Suppress "replacing ..." message.
    chain_mod <- suppressMessages(bbr::update_model_id(chain_mod))
    return(chain_mod)
  })
  bbr::submit_models(mods, ...)
}

get_est_method <- function(record) {
  meth <- nmrec::get_record_option(record, "method")
  if (is.null(meth)) {
    stop("Estimation record must specify method option:\n",
         format(record))
  }
  return(tolower(meth$value))
}

adjust_data_path <- function(ctl) {
  data_recs <- nmrec::select_records(ctl, "data")
  if (!length(data_recs)) {
    stop("No data records found")
  }

  opts <- purrr::list_c(purrr::map(data_recs, function(r) r$get_options()))
  file_opts <- purrr::keep(opts, function(o) o$name == "filename")
  if (length(file_opts) != 1) {
    stop("Expected single filename option for $data")
  }

  filename <- file_opts[[1]]$value
  if (grepl("^'.+'$", filename) || grepl('^".+"$', filename)) {
    quote_char <- substr(filename, 1, 1)
    filename <- substr(filename, 2, nchar(filename) - 1)
    quote_fn <- function(x) paste0(quote_char, x, quote_char)
  } else {
    quote_fn <- identity
  }

  if (!fs::is_absolute_path(filename)) {
    file_opts[[1]]$value <- quote_fn(file.path("..", filename))
  }

  return(invisible(NULL))
}

set_chain_file <- function(record) {
  file_opts <- purrr::keep(record$get_options(), function(o) o$name == "file")
  init_file <- file.path("..", "init.chn")
  if (length(file_opts) > 1) {
    stop("method=chain estimation record has more than one file option")
  } else if (!length(file_opts)) {
    record$values <- c(record$values, sprintf("  FILE=%s\n", init_file))
  } else {
    file_opts[[1]]$value <- init_file
  }

  return(invisible(NULL))
}
