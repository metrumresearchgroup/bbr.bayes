skip_long_tests("long-running nm_join_bayes() tests")
skip_if_no_bbi()

withr::local_options(list(
  bbr.bbi_exe_path = bbr::read_bbi_path(),
  bbr.verbose = FALSE
))

testthat::skip_if_not_installed("future.apply")

testthat::skip_if_not_installed("mrgsolve")
if (packageVersion("mrgsolve") < "1.2.0") {
  testthat::skip("mrgsolve >= 1.2.0 required to get different draws for EPS")
}

get_mrgsolve_model <- function() {
  capture.output(type = "message", {
    mod <- mrgsolve::mread(
      system.file("model", "mrgsolve", "1100.mod",
        package = "bbr.bayes", mustWork = TRUE
      )
    )
  })
  return(mod)
}

MOD_MS <- get_mrgsolve_model()

# Note: Avoid dropping this too low because it will trigger a 'not positive
# definite' npde error due to having fewer posterior samples than observations
# per subject.
N_POST <- 30

table_columns <- function() {
  fread_peek_at_columns(
    system.file("model", "nonmem", "bayes", "1100", "1100-1", "1100.tab",
      package = "bbr.bayes", mustWork = TRUE
    )
  )
}

read_mod_data <- function() {
  d <- data.table::fread(
    system.file("extdata", "analysis3.csv",
      package = "bbr.bayes", mustWork = TRUE
    ),
    na.strings = ".", verbose = FALSE, data.table = FALSE
  )
  tibble::as_tibble(d)
}

test_that("nm_join_bayes() aborts: ewres_npde=TRUE without epred=TRUE", {
  testthat::skip_if_not_installed("npde")

  expect_error(
    nm_join_bayes(NMBAYES_MOD1, MOD_MS, epred = FALSE, ewres_npde = TRUE),
    "epred = TRUE"
  )
})

test_that("nm_join_bayes() aborts: invalid presim_fn return", {
  expect_error(
    nm_join_bayes(NMBAYES_MOD1, MOD_MS, presim_fn = function(...) 1),
    "must return a data frame"
  )
  expect_error(
    nm_join_bayes(NMBAYES_MOD1, MOD_MS, presim_fn = function(d) d[-1, ]),
    "had .* rows"
  )
  expect_error(
    nm_join_bayes(
      NMBAYES_MOD1, MOD_MS,
      presim_fn = function(d) dplyr::select(d, -"NUM")
    ),
    "must retain join column"
  )
})

test_that("nm_join_bayes() aborts: DV not present in result", {
  expect_error(
    nm_join_bayes(
      NMBAYES_MOD1, MOD_MS,
      presim_fn = function(d) {
        dplyr::rename(d, LDV = "DV")
      }
    ),
    "specify a column remapped to DV"
  )
})

test_that("nm_join_bayes() aborts: join_col not in data", {
  expect_error(
    nm_join_bayes(NMBAYES_MOD1, MOD_MS, .join_col = "FOOBAR"),
    "FOOBAR"
  )
})

test_that("nm_join_bayes() aborts: absolutes paths for .files", {
  expect_error(
    nm_join_bayes(
      NMBAYES_MOD1, MOD_MS,
      .files = file.path(getwd(), "foo.tab")
    ),
    "must be relative paths"
  )
})

test_that("nm_join_bayes() works", {
  testthat::skip_if_not_installed("npde")

  withr::with_seed(3012, {
    res <- nm_join_bayes(NMBAYES_MOD1, MOD_MS,
      n_post = N_POST,
      ewres_npde = FALSE
    )
  })

  expect_s3_class(res, "tbl_df")

  data <- read_mod_data()

  expect_length(setdiff(names(data), names(res)), 0)
  expect_setequal(
    names(res),
    c(
      table_columns(), names(data), "DV.DATA",
      "EPRED_lo", "EPRED_hi",
      "IPRED_lo", "IPRED_hi"
    )
  )

  is_blq <- data$BLQ != 0
  expect_equal(nrow(res), sum(!is_blq))

  expect_equal(sum(is.na(res$NUM)), 0)
  expect_equal(dplyr::n_distinct(res$ID), 160)

  # Do another to check other features and to test that an additional run gives
  # the same result. This is grouping a good number of things together to
  # minimize relatively slow non-quick nm_join_bayes() calls.

  ipred_path <- withr::local_tempfile()
  withr::with_seed(3012, {
    res2_full <- nm_join_bayes(
      NMBAYES_MOD1, MOD_MS,
      .files = paste0(get_model_id(NMBAYES_MOD1), ".tab"),
      .superset = TRUE,
      .bbi_args = list(no_grd_file = TRUE),
      probs = c(0.2, 0.8),
      n_post = N_POST,
      ipred_path = ipred_path,
      ewres_npde = TRUE,
      presim_fn = function(d) {
        dplyr::rename(d, H = "HT")
      }
    )
  })

  # .superset=TRUE gives all rows in data set, in same order.

  expect_identical(as.integer(res2_full$NUM), data$NUM)
  expect_gt(sum(is_blq), 0)
  expect_true(all(is.na(res2_full$IPRED[is_blq])))

  res2 <- res2_full[!is_blq, ]
  expect_identical(dim(res2), dim(res))

  # Most values are the same for the results of the first and second
  # nm_join_bayes() calls. The exceptions are
  #
  #  * the EPRED and IPRED bounds change due to the non-default probs in the
  #    second call
  #
  #  * presim_fn in the second call renamed HT to H
  #
  #  * EWRES/NPDE differ due to the different ewres_npde values
  changed_cols <- c(
    "EPRED_lo", "EPRED_hi", "IPRED_lo", "IPRED_hi",
    "EWRES", "NPDE"
  )

  expect_equal(
    dplyr::select(res, -all_of(changed_cols)),
    dplyr::select(res2, -all_of(changed_cols)) %>%
      dplyr::rename(HT = "H")
  )

  # Narrower intervals were specified (via probs) for res2.
  notna <- !is.na(res$EPRED)
  expect_true(all(res$EPRED_lo[notna] < res2$EPRED_lo[notna]))
  expect_true(all(res$EPRED_hi[notna] > res2$EPRED_hi[notna]))
  expect_true(all(res$IPRED_lo[notna] < res2$IPRED_lo[notna]))
  expect_true(all(res$IPRED_hi[notna] > res2$IPRED_hi[notna]))

  # res used ewres_npde=FALSE, while res2 used ewres_npde=TRUE, so the values
  # should not match.
  expect_false(all(dplyr::near(res$EWRES, res2$EWRES)))
  expect_false(all(dplyr::near(res$NPDE, res2$NPDE)))

  # Simulated IPRED values written to the path passed to ipred_path.
  ipred <- readr::read_csv(ipred_path)
  expect_identical(names(ipred), c("NUM", "DV_sim", "sample"))
})

test_that("nm_join_bayes_quick() args are strict subset of nm_join_bayes()", {
  args <- formals(nm_join_bayes)
  args_quick <- formals(nm_join_bayes_quick)

  arg_names <- names(args)
  arg_names_quick <- names(args_quick)

  expect_identical(
    arg_names_quick,
    intersect(arg_names, arg_names_quick)
  )
  expect_identical(
    args_quick[arg_names_quick],
    args[arg_names_quick]
  )
})

test_that("nm_join_bayes_quick() works", {
  res <- nm_join_bayes_quick(NMBAYES_MOD1)

  tabs <- purrr::map(chain_paths(NMBAYES_MOD1, "1100", ".tab"), fread_chain_file) %>%
    dplyr::bind_rows(.id = "chain")

  # A few random spot checks.
  for (num in c(102, 2969, 3404, 4140)) {
    expect_equal(
      dplyr::filter(res, .data$NUM == num)$EPRED,
      stats::median(tabs[tabs$NUM == num, ]$EPRED)
    )
    expect_equal(
      dplyr::filter(res, .data$NUM == num)$EPRED,
      stats::median(tabs[tabs$NUM == num, ]$EPRED)
    )
  }

  expect_s3_class(res, "tbl_df")

  data <- read_mod_data()

  expect_length(setdiff(names(data), names(res)), 0)
  tab_cols <- table_columns()
  expect_setequal(
    names(res),
    c(tab_cols, names(data), "DV.DATA")
  )

  expect_equal(nrow(res), sum(data$BLQ == 0))

  expect_equal(sum(is.na(res$NUM)), 0)
  expect_equal(dplyr::n_distinct(res$ID), 160)

  res2 <- nm_join_bayes_quick(NMBAYES_MOD1,
    point_fn = function(x) 10 * stats::median(x)
  )
  expect_identical(names(res), names(res2))
  expect_equal(
    dplyr::select(res, -any_of(tab_cols)),
    dplyr::select(res2, -any_of(tab_cols))
  )
  expect_equal(res2$IPRED, 10 * res$IPRED)
})

test_that("nm_join_bayes_quick() supports .superset=TRUE", {
  res <- nm_join_bayes_quick(NMBAYES_MOD1, .superset = TRUE)
  data <- read_mod_data()

  expect_equal(res$NUM, data$NUM)
  expect_in(names(data), names(res))
})
