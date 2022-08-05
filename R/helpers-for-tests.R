
#' Skip test if not running Stan tests
#'
#' First checks if `cmdstanr` is installed, then if the `SKIP_STAN_TESTS`
#' environment variable is set.
#' @param .test_name Character scalar to identify the test being potentially skipped.
#'   This is printed in the skip message
#' @keywords internal
skip_if_no_stan <- function(.test_name) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    testthat::skip(paste("Skipping", .test_name, "because `cmdstanr` is not installed."))
  }

  if (Sys.getenv("SKIP_STAN_TESTS") == "true") {
    testthat::skip(paste("Skipping", .test_name, "because SKIP_STAN_TESTS set to 'true'"))
  }
}

#' Skip long-running tests
#'
#' For example, tests that actual submit models to be run.
#' @param .explanation Reason for skipping tests, or description of tests being skipped
#' @keywords internal
skip_long_tests <- function(.explanation = "Skipping long running tests") {
  if (Sys.getenv("SKIP_LONG_TESTS") == "true") {
    testthat::skip(.explanation)
  }
}
