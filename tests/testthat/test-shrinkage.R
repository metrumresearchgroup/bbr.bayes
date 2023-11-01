
### Example project reference comparison

read_chain_files <- function(mod, extension) {
  files <- get_chain_files(mod, extension)
  names(files) <- seq_along(files)
  purrr::map_dfr(files, fread_chain_file, .id = "chain") %>%
    dplyr::filter(.data$ITERATION > 0)
}

# Reference implementation derived from the example project's baysh function.
shrinkage_ref_impl <- function(mod, use_sd = TRUE) {
  ext <- read_chain_files(mod, "ext")
  iph <- read_chain_files(mod, "iph")

  numer <- dplyr::select(iph,
                         "chain", "ITERATION", "ID",
                         starts_with("ETA")) %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::select("ID", starts_with("ETA")) %>%
    dplyr::summarise(dplyr::across(everything(), mean),
                     .groups = "drop") %>%
    dplyr::select(starts_with("ETA")) %>%
    dplyr::summarise(dplyr::across(everything(),
                                   if (isTRUE(use_sd)) sd else var))

  eta_idxs <- stringr::str_extract(names(numer), "\\d+")
  omegas <- purrr::map_chr(eta_idxs, ~ glue("OMEGA({.x},{.x})"))
  denom <- ext %>%
    dplyr::select(all_of(omegas)) %>%
    dplyr::summarise(dplyr::across(everything(), mean))
  if (isTRUE(use_sd)) {
    denom <- sqrt(denom)
  }

  res <- as.numeric(1 - numer / denom) * 100
  names(res) <- names(numer)
  return(res)
}

test_that("shrinkage.bbi_nmbayes_model() matches reference implementation: sd", {
  expect_equal(shrinkage(NMBAYES_MOD1),
               unname(shrinkage_ref_impl(NMBAYES_MOD1)))
})

test_that("shrinkage.bbi_nmbayes_model() matches reference implementation: var", {
  expect_equal(shrinkage(NMBAYES_MOD1, use_sd = FALSE),
               unname(shrinkage_ref_impl(NMBAYES_MOD1, use_sd = FALSE)))
})


test_that("shrinkage.rvar() 1d matches ref", {
  ref <- unname(shrinkage_ref_impl(NMBAYES_MOD1))
  draws <- posterior::as_draws_rvars(NMBAYES_MOD1)
  etas <- posterior::drop(draws[["ETA"]])
  omega_diag <- diag(draws[["OMEGA"]])
  for (i in seq_along(ref)) {
    expect_equal(shrinkage(posterior::drop(etas[i, ]),
                           variance = omega_diag[i]),
                 ref[i])
  }
})

test_that("shrinkage.rvar() 3d+ matches ref", {
  ref <- unname(shrinkage_ref_impl(NMBAYES_MOD1))
  draws <- posterior::as_draws_rvars(NMBAYES_MOD1)
  eta <- draws[["ETA"]]
  omega_diag <- diag(draws[["OMEGA"]])
  dim(omega_diag) <- dim(eta)[-3]
  res <- shrinkage(eta, variance = omega_diag)
  # Unlike shrinkage.bbi_nmbayes_model(), this doesn't drop redundant
  # dimensions, so shrinkage() operated over the last dimension of a 3d array.
  expect_identical(dim(res), c(5L, 1L))
  # But the underlying values line up.
  expect_equal(as.numeric(res), ref)

  # Extend rvars with an additional dimension duplicating same values. Each one
  # should match the ref.
  eta2 <- c(eta, eta)
  dim(eta2) <- c(dim(eta), 2)
  omega_diag2 <- c(omega_diag, omega_diag)
  dim(omega_diag2) <- c(dim(omega_diag), 2)

  res <- shrinkage(eta2, variance = omega_diag2, group_idx = 3)
  expect_identical(dim(res), c(5L, 1L, 2L))
  expect_equal(as.numeric(res[, , 1]), ref)
  expect_equal(as.numeric(res[, , 2]), ref)
})

### Gelman & Pardoe 2006 reference implementation

# Pooling factor based on code in Appendix B of Gelman & Pardoe 2006.
#
# e is a matrix of samples, draws by groups.
gp_lambda_var <- function(e) {
  1 - var(apply(e, 2, mean)) / mean(apply(e, 1, var))
}

# Like gp_lambda_var, but modified to use sd() instead of var().
gp_lambda_sd <- function(e) {
  1 - sd(apply(e, 2, mean)) / mean(apply(e, 1, sd))
}

test_that("shrinkage.rvar() matches Gelman & Pardoe reference implementation", {
  x <- rnorm(50)
  dim(x) <- c(10, 5)

  # It's not necessary, but shift a couple of columns to get things away from
  # the upper bound.
  x[, 2] <- 1 + x[, 2]
  x[, 5] <- -2 + x[, 5]

  expect_equal(shrinkage(posterior::rvar(x), use_sd = FALSE),
               gp_lambda_var(x) * 100)
  expect_equal(shrinkage(posterior::rvar(x), use_sd = TRUE),
               gp_lambda_sd(x) * 100)

  y <- rnorm(150)
  dim(y) <- c(10, 5, 3)

  y[, 2, ] <- 1 + y[, 2, ]
  y[, 5, ] <- -2 + y[, 5, ]

  res_var <- shrinkage(posterior::rvar(y), group_idx = 1, use_sd = FALSE)
  expect_length(res_var, 3L)
  expect_equal(res_var,
               apply(y, 3, gp_lambda_var) * 100)

  res_sd <- shrinkage(posterior::rvar(y), group_idx = 1, use_sd = TRUE)
  expect_length(res_sd, 3L)
  expect_equal(res_sd,
               apply(y, 3, gp_lambda_sd) * 100)
})

### Other tests

test_that("shrinkage(): rank matches for use_sd=TRUE and use_sd=FALSE", {
  res_sd <- shrinkage(NMBAYES_MOD1, use_sd = TRUE)
  res_var <- shrinkage(NMBAYES_MOD1, use_sd = FALSE)
  expect_false(any(res_sd == res_var))
  expect_identical(rank(res_sd), rank(res_var))
})

test_that("shrinkage.bbi_nmbayes_model() falls back to *.shk files", {
  tdir <- withr::local_tempdir("bbr-bayes-")
  modfile <- get_model_path(NMBAYES_MOD1)
  fs::file_copy(modfile, tdir)
  fs::file_copy(get_yaml_path(NMBAYES_MOD1), tdir)

  mod_id <- get_model_id(NMBAYES_MOD1)
  rundir <- file.path(tdir, mod_id)
  fs::dir_copy(fs::path_ext_remove(modfile),
               rundir)

  mod <- read_model(rundir)
  iphs <- get_chain_files(mod, ".iph", check_exists = "all")
  if (!all(fs::path_has_parent(iphs, get_model_working_directory(mod)))) {
    fail(glue("Returned files are not under expected directory",
              " - directory: {get_model_working_directory(mod)}",
              " - files:     {files}",
              files = paste(iphs, collapse = ", "),
              .sep = "\n"))
    return(NULL)
  }
  fs::file_delete(iphs)

  res_shk <- shrinkage(mod)
  res_iph <- shrinkage(NMBAYES_MOD1)
  expect_false(any(res_shk == res_iph))
  expect_identical(dim(res_shk), dim(res_iph))
  # The results should still be fairly similar.
  expect_lt(mean(abs(res_shk - res_iph)), 5)

  # Error cases

  shk_files <- fs::path_ext_set(iphs, "shk")

  # No TYPE=4 row
  for (f in shk_files) {
    # Drop TYPE=4 value.
    lines <- sub(" 4 ", " 1 ", readLines(f))
    writeLines(lines, f)
  }
  expect_error(shrinkage(mod), "TYPE=4")

  # No TYPE column
  for (f in shk_files) {
    readr::write_delim(tibble::tibble(foo = 1:2, bar = 3:4), f, delim = " ")
  }
  expect_error(shrinkage(mod), "extract TYPE column")

  fs::file_delete(shk_files)
  expect_error(shrinkage(mod), "cannot calculate")
})

expect_percent <- function(x, len = NULL) {
  res <- checkmate::check_double(x, lower = 0, upper = 100, len = len)
  checkmate::makeAssertion(
    x, res, var.name = checkmate::vname(x), collection = NULL)
}

test_that("shrinkage.draws() and shrinkage.rvar() return same value", {
  x <- posterior::rvar_rng(rnorm, 10, ndraws = 60)
  res <- shrinkage.draws(posterior::as_draws_list(list(foo = x)),
                         "foo")
  expect_percent(res)
  expect_equal(res, shrinkage.rvar(x))
})

test_that("shrinkage.rvar() returns expected shape", {
  # One-dimensional rvar with 10 "groups".
  x <- posterior::rvar_rng(rnorm, 10, ndraws = 60)
  res <- shrinkage(x)
  expect_percent(res, len = 1L)
  expect_null(dim(res))

  # Two-dimensional rvar with 10 groups...
  x <- posterior::rvar_rng(rnorm, 50, ndraws = 60)
  dim(x) <- c(5, 10)
  res <- shrinkage(x)
  expect_percent(res, len = 5L)
  expect_null(dim(res))
  # ... or 5 groups.
  res <- shrinkage(x, group_idx = 1)
  expect_percent(res, len = 10L)
  expect_null(dim(res))

  # Three-dimensional rvar with 10 groups...
  x <- posterior::rvar_rng(rnorm, 100, ndraws = 60)
  dim(x) <- c(2, 5, 10)
  res <- shrinkage(x)
  expect_percent(res)
  expect_identical(dim(res), c(2L, 5L))
  # ... or 5 groups...
  res <- shrinkage(x, group_idx = 2)
  expect_percent(res)
  expect_identical(dim(res), c(2L, 10L))
  # ... or 2 groups...
  res <- shrinkage(x, group_idx = 1)
  expect_percent(res)
  expect_identical(dim(res), c(5L, 10L))
  # ... or 50 groups across two dimensions.
  res <- shrinkage(x, group_idx = c(2, 3))
  expect_percent(res)
  expect_null(dim(res))
  expect_length(res, 2L)

  # Redundant dimensions are kept.
  x <- posterior::rvar_rng(rnorm, 100, ndraws = 60)
  dim(x) <- c(50, 1, 2)
  res <- shrinkage(x, group_idx = 1)
  expect_percent(res)
  expect_identical(dim(res), c(1L, 2L))
})

test_that("shrinkage.rvar() aborts on invalid input", {
  x <- posterior::rvar_rng(rnorm, 10, ndraws = 60)
  y <- x
  dim(y) <- c(2, 5)

  expect_equal(dim(x), 10L)
  expect_error(shrinkage(x, group_idx = 1),
               "margin free")

  expect_error(shrinkage(y, group_idx = 3),
               "group_idx values")

  expect_error(shrinkage(x, variance = y),
               "dim(`variance`)",
               fixed = TRUE)
})

test_that("shrinkage.draw() aborts if errors_name is not found", {
  draws <- posterior::example_draws(example = "eight_schools")
  expect_error(shrinkage(draws, "not-there"), "in rvar draws")
})
