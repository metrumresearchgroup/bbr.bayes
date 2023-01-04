
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

test_that("shrinkage.rvar() aborts on invalid input", {
  x <- posterior::rvar_rng(rnorm, 10, ndraws = 50)
  y <- x
  dim(y) <- c(2, 5)

  expect_equal(dim(x), 10L)
  expect_error(shrinkage(x, variance = y, group_idx = 1),
               "margin free")

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
