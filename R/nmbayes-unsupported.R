#' @export
cov_cor.bbi_nmbayes_model <- function(...) {
  stop("cov_cor() is not supported for nmbayes models.")
}

#' @export
nm_file.bbi_nmbayes_model <- function(...) {
  stop(
    "nm_file() is not supported for nmbayes models; ",
    "instead get paths with `chain_paths()` and then read manually."
  )
}
