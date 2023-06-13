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
  ctl <- readr::read_lines(ctl_file)

  row_bayes <- stringr::str_detect(ctl, "METHOD=BAYES|METHOD=NUTS")
  est_bayes <- ctl[row_bayes]
  est_bayes <- stringr::str_replace(est_bayes, "^;", "")
  ctl[row_bayes] <- est_bayes

  row_table <- stringr::str_detect(ctl, ";\\s*\\$TABLE")
  block_table <- ctl[row_table]
  block_table <- stringr::str_replace(block_table, "^;", "")
  ctl[row_table] <- block_table

  row_chain <- stringr::str_detect(ctl, "METHOD=CHAIN")
  est_chain <- ctl[row_chain]
  n_chain <- as.numeric(stringr::str_extract(est_chain, "(?<=NSAMPLE=)[0-9]+"))
  est_chain <- stringr::str_replace(est_chain, "NSAMPLE=[0-9]+", "NSAMPLE=0")
  est_chain <- stringr::str_replace(est_chain, "FILE=", "FILE=../")

  row_data <- stringr::str_detect(ctl, "\\$DATA")
  data_record <- ctl[row_data]
  ctl[row_data] <- stringr::str_replace(
    data_record,
    "\\$DATA\\s+",
    "$DATA ../")

  row_extrasend <- stringr::str_detect(ctl, "extrasend")
  ctl[row_extrasend] <- stringr::str_replace(
    ctl[row_extrasend],
    "extrasend",
    "../extrasend")

  .run <- get_model_id(.mod)
  outdir <- get_output_dir(.mod)
  # Use same extension for chain submodels so that bbi treats the data path the
  # same way.
  ext <- fs::path_ext(ctl_file)
  mods <- purrr::map(seq_len(n_chain), function(.chain) {
    est_chain_i <- stringr::str_replace(
      est_chain,
      "ISAMPLE=0",
      glue("ISAMPLE={.chain}"))
    est_bayes_i <- stringr::str_replace(
      est_bayes,
      "SEED=[0-9]+",
      glue("SEED={.chain}"))
    ctl_i <- ctl
    ctl_i[row_chain] <- est_chain_i
    ctl_i[row_bayes] <- est_bayes_i
    readr::write_lines(ctl_i, file.path(
      outdir,
      glue("{.run}-{.chain}.{ext}"))
    )

    new_model(
      file.path(outdir, glue("{.run}-{.chain}")),
      .description = glue("Chain {.chain}"),
    )
  })
  bbr::submit_models(mods, ...)
}
