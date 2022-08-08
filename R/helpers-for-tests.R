
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
