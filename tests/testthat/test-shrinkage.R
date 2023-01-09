
read_chain_files <- function(mod, extension) {
  files <- get_chain_files(mod, extension)
  names(files) <- seq_along(files)
  purrr::map_dfr(files, fread_chain_file, .id = "chain") %>%
    dplyr::filter(.data$ITERATION > 0)
}

# Reference implementation derived from the example project's baysh function.
shrinkage_ref_impl <- function(mod) {
  ext <- read_chain_files(mod, "ext")
  iph <- read_chain_files(mod, "iph")

  numer <- dplyr::select(iph,
                       "chain", "ITERATION", "ID",
                       tidyselect::starts_with("ETA")) %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::select("ID", tidyselect::starts_with("ETA")) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(), mean),
                     .groups = "drop") %>%
    dplyr::select(tidyselect::starts_with("ETA")) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(), sd))

  eta_idxs <- stringr::str_extract(names(numer), "\\d+")
  omegas <- purrr::map_chr(eta_idxs, ~ glue("OMEGA({.x},{.x})"))
  denom <- ext %>%
    dplyr::select(tidyr::all_of(omegas)) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(),
                                   ~ sqrt(mean(.x))))

  res <- as.numeric(1 - numer / denom) * 100
  names(res) <- names(numer)
  return(res)
}

test_that("shrinkage.bbi_nmbayes_model() matches reference implementation", {
  expect_equal(shrinkage(NMBAYES_MOD1),
               unname(shrinkage_ref_impl(NMBAYES_MOD1)))
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

  invalid_data <- tibble::tibble(TYPE = 1:2)
  for (f in shk_files) {
    readr::write_csv(invalid_data, f)
  }
  expect_error(shrinkage(mod), "TYPE=4")

  fs::file_delete(shk_files)
  expect_error(shrinkage(mod), "cannot calculate")
})

expect_percent <- function(x, len = NULL) {
  res <- checkmate::check_double(x, lower = 0, upper = 100, len = len)
  checkmate::makeAssertion(
    x, res, var.name = checkmate::vname(x), collection = NULL)
}

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

test_that("shrinkage.draws() aborts on invalid input", {
  draws <- posterior::as_draws(NMBAYES_MOD1)
  expect_error(shrinkage(draws,
                         errors_name = "ETA", variance_name = "ETA",
                         from_diag = TRUE),
               "must be matrix")
})
