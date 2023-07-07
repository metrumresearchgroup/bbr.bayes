
skip_if_no_bbi <- function() {
  if (!(nzchar(Sys.which("bbi")) || nzchar(Sys.getenv("BBI_EXE_PATH")))) {
    testthat::skip("Requires bbi executable")
  }
}

# A short stretch of jsonlite versions use 17 significant digits instead of 15
# when `digits = NA`. The data in this test suite was generated with 15-digit
# precision.
jsonlite_prec_change <- function() {
  v <- packageVersion("jsonlite")
  return(v > "1.8.4" && v < "1.8.7")
}

skip_if_jsonlite_prec_change <- function() {
  if (jsonlite_prec_change()) {
    testthat::skip("Installed jsonlite version uses different max precision")
  }
}
