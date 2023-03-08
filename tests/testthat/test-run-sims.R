
skip_long_tests("skipping long-running run_sim() tests")

test_that("run_sims() works", {
  capture.output(type = "message", {
    mod_ms <- mrgsolve::mread(
      system.file("model", "mrgsolve", "1100.mod",
                  package = "bbr.bayes", mustWork = TRUE))
  })

  withr::with_seed(3012, {
    res <- run_sims(NMBAYES_MOD1, mod_ms, n_post = 10)
  })

  expect_s3_class(res, "tbl_df")
  expect_setequal(
    names(res),
    c("NUM", "chain", "CL", "V2", "Q", "V3", "KA",
      "ETA1", "ETA2", "ETA3", "ETA4", "ETA5",
      "NPDE", "EWRES", "PRED", "RES", "WRES",
      "EPRED", "EPRED_lo", "EPRED_hi",
      "IPRED", "IPRED_lo", "IPRED_hi"))

  # TODO: extend

  # TODO: tmp dev drop
  tfile <- withr::local_tempfile()
  readr::write_csv(res, tfile)
  fs::file_copy(tfile, "../../ref.csv", overwrite = TRUE)
  expect_identical(unname(tools::md5sum(tfile)),
                   "8854b268e6ca35f276977cdebe1f73af")
  # end tmp dev

  ipred_path <- withr::local_tempfile()
  withr::with_seed(3012, {
    res2 <- run_sims(NMBAYES_MOD1, mod_ms, n_post = 10,
                     ipred_path = ipred_path)
  })

  expect_equal(res, res2)

  # TODO: more extensive checking
  ipred <- readr::read_csv(ipred_path)
  expect_identical(names(ipred), c("NUM", "DV_sim", "sample"))
})

## Error in `decorr.chol(varsim)`: object 'ymat' not found
#
# test_that("run_sims() optionally runs autonpde", {
#   capture.output(type = "message", {
#     mod_ms <- mrgsolve::mread(
#       system.file("model", "mrgsolve", "1100.mod",
#                   package = "bbr.bayes", mustWork = TRUE))
#   })
#
#   withr::with_seed(3012, {
#     res <- run_sims(NMBAYES_MOD1, mod_ms, n_post = 10,
#                     ewres_npde = TRUE)
#   })
# })
