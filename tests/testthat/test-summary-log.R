skip_if_no_bbi()

withr::local_options(list(bbr.bbi_exe_path = bbr::read_bbi_path()))

test_that("summary_log() relays model_summary() errors", {
  sl <- summary_log(
    system.file("model", package = "bbr.bayes", mustWork = TRUE),
    .recurse = TRUE
  )
  expect_setequal(
    sl[[RUN_ID_COL]],
    c(
      NM_MOD_ID,
      NMBAYES_MOD_ID,
      STAN_MOD_ID, STAN_MOD_ID3, STAN_GQ_MOD_ID
    )
  )

  no_summary <- purrr::map_lgl(sl[[SL_SUMMARY]], is.null)
  expect_identical(sl[!no_summary, ][[RUN_ID_COL]], NM_MOD_ID)
  expect_setequal(
    sl[no_summary, ][[RUN_ID_COL]],
    c(
      NMBAYES_MOD_ID,
      STAN_MOD_ID, STAN_MOD_ID3, STAN_GQ_MOD_ID
    )
  )

  expect_match(
    sl[no_summary, ][["error_msg"]],
    "read_fit_model",
    all = TRUE
  )
})
