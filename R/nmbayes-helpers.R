
#' Return a nmbayes model's submodel directories
#'
#' Each directory corresponds to a chain and is expected to be named in the
#' "{id}_{chain}" format created by [run_chains()].
#'
#' @param .mod A bbi_nmbayes_model object.
#' @return A character vector of absolute paths to the submodels, sorted in
#'   ascending order by chain number.
#' @noRd
get_chain_dirs <- function(.mod) {
  outdir <- get_output_dir(.mod)
  mid <- get_model_id(.mod)
  chaindirs <- list.files(outdir,
                          pattern = paste0("^\\Q", mid, "\\E_[0-9]+$"),
                          full.names = TRUE)

  nchains <- length(chaindirs)
  if (nchains == 0) {
    stop("No chains found in ", outdir, call. = FALSE)
  }

  chains <- sort(as.integer(stringr::str_extract(chaindirs, "[0-9]+$")))
  if (!identical(seq_len(nchains), chains)) {
    stop("Expected 1:", nchains, " chains, got:\n",
         paste(chains, collapse = ", "),
         call. = FALSE)
  }

  return(chaindirs[chains])
}

get_chain_exts <- function(.mod) {
  dirs <- get_chain_dirs(.mod)
  return(file.path(dirs, paste0(basename(dirs), ".ext")))
}

fread_chain_file <- function(file) {
  tibble::as_tibble(
    data.table::fread(file = file,
                      na.strings = ".",
                      data.table = FALSE,
                      verbose = FALSE))
}

#' Rename NONMEM variables for rvar compatibility
#'
#' NONMEM uses labels like `THETA1` and `OMEGA(1,1)`for its flattened variables.
#' \pkg{posterior}, on the other hand, constructs its random variable data type
#' with labels that use square brackets, e.g., `THETA[1]` or `OMEGA[1,1]`.
#'
#' @noRd
rename_nm_as_rvar  <- function(name) {
  stringr::str_replace(name, "(.*)([0-9]+)$", "\\1[\\2]") %>%
    stringr::str_replace("(.*)\\(([,0-9]+)\\)$", "\\1[\\2]")
}
