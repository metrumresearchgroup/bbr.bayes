
skip_if_no_bbi <- function() {
  if (!(nzchar(Sys.which("bbi")) || nzchar(Sys.getenv("BBI_EXE_PATH")))) {
    testthat::skip("Requires bbi executable")
  }
}
