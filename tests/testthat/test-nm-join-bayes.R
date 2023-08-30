
skip_long_tests("long-running nm_join_bayes() tests")
testthat::skip_if_not_installed("mrgsolve")
testthat::skip_if_not_installed("future.apply")

if (packageVersion("mrgsolve") < "1.2.0") {
  testthat::skip("mrgsolve >= 1.2.0 required to get different draws for EPS")
}

get_mrgsolve_model <- function() {
  capture.output(type = "message", {
    mod <- mrgsolve::mread(
      system.file("model", "mrgsolve", "1100.mod",
                  package = "bbr.bayes", mustWork = TRUE))
  })
  return(mod)
}

MOD_MS <- get_mrgsolve_model()

read_mod_data <- function() {
  d <- data.table::fread(
    system.file("extdata", "analysis3.csv",
                package = "bbr.bayes", mustWork = TRUE),
    na.strings = ".", verbose = FALSE, data.table = FALSE)
  tibble::as_tibble(d)
}

test_that("nm_join_bayes() aborts: ewres_npde=TRUE without epred=TRUE", {
  testthat::skip_if_not_installed("npde")

  expect_error(
    nm_join_bayes(NMBAYES_MOD1, MOD_MS, epred = FALSE, ewres_npde = TRUE),
    "epred = TRUE")
})

test_that("nm_join_bayes() aborts: join_col not in data", {
  expect_error(
    nm_join_bayes(NMBAYES_MOD1, MOD_MS, join_col = "foobar"),
    "foobar")
})

test_that("nm_join_bayes() aborts: join_col not in table", {
  data <- read_mod_data()
  data[, "FOO"] <- data$NUM
  expect_error(
    nm_join_bayes(NMBAYES_MOD1, MOD_MS, join_col = "FOO", data = data),
    ".tab",
    fixed = TRUE)
})

test_that("nm_join_bayes() aborts: data and table name collision", {
  data <- read_mod_data()
  data[, "EWRES"] <- 1
  expect_error(
    nm_join_bayes(NMBAYES_MOD1, MOD_MS, data = data),
    "data names collide")
})

test_that("nm_join_bayes() aborts: ipred=TRUE aborts on model without iph files", {
  mod <- local_nmbayes_model_no_iph()
  data <- read_mod_data()
  expect_error(
    nm_join_bayes(mod, MOD_MS, epred = FALSE, ipred = TRUE, data = data),
    "requires iph files")
})

test_that("nm_join_bayes() works", {
  withr::with_seed(3012, {
    res <- nm_join_bayes(NMBAYES_MOD1, MOD_MS, n_post = 10)
  })

  expect_s3_class(res, "tbl_df")

  data <- read_mod_data()

  expect_length(setdiff(names(data), names(res)), 0)
  expect_setequal(
    setdiff(names(res), names(data)),
    c("NPDE", "EWRES", "PRED", "RES", "WRES",
      "EPRED", "EPRED_lo", "EPRED_hi",
      "IPRED", "IPRED_lo", "IPRED_hi"))

  expect_equal(nrow(res), sum(data$BLQ == 0))

  expect_equal(sum(is.na(res$NUM)), 0)
  expect_equal(dplyr::n_distinct(res$ID), 160)

  # TODO: This check just makes sure the output isn't changing unexpectedly
  # during development. The plan is to replace it with a hash check for some
  # result(s) inspected by SMEs.
  tfile <- withr::local_tempfile()
  readr::write_csv(res, tfile)
  expect_identical(unname(tools::md5sum(tfile)),
                   "9eed108984109825c70db2efe357b757")

  ipred_path <- withr::local_tempfile()
  withr::with_seed(3012, {
    res2 <- nm_join_bayes(NMBAYES_MOD1, MOD_MS, n_post = 10,
                          ipred_path = ipred_path)
  })

  expect_equal(res, res2)

  ipred <- readr::read_csv(ipred_path)
  expect_identical(names(ipred), c("NUM", "DV_sim", "sample"))

  # TODO: Add more extensive checks of results.

  # TODO: Cover other arguments.
})

test_that("nm_join_bayes() optionally runs autonpde", {
  withr::with_seed(3012, {
    # Note: if n_post is dropped to a lower value (e.g., 10 or 20), the
    # decorr.chol call underneath fails, even with mrgsolve's 07793922 (all
    # records get a different draw from EPS, 2023-08-22).
    res <- nm_join_bayes(NMBAYES_MOD1, MOD_MS, n_post = 25,
                         ewres_npde = TRUE)
  })

  expect_s3_class(res, "tbl_df")
})
