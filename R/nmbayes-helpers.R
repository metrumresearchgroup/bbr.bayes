
nmbayes_warn <- function() {
  if (!identical(Sys.getenv("TESTTHAT"), "true")) {
    warning("NONMEM Bayes support is incomplete, ",
            "and backward incompatible changes are planned.")
  }
}

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
                          pattern = paste0("^\\Q", mid, "\\E-[0-9]+$"),
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

#' Return `{run}.{extension}` file from each chain subdirectory.
#'
#' @param .mod A `bbi_nmbayes_model`.
#' @param extension File extension.
#' @param chain_dirs Paths to chain subdirectories. This argument exists so that
#'   callers can avoid repeated calls to `get_chain_dirs()`; the value should
#'   match what is returned by `get_chain_dirs()`.
#' @param check_exists Whether to check that the files exist. If "all", confirm
#'   that each chain subdirectory has a `{run}.{extension}` file. If
#'   "all_or_none", do the same, but, if the file doesn't exist in any
#'   subdirectory, return an empty character rather than aborting. Specify "no"
#'   to disable the check entirely.
#' @return Absolute file paths, one for each chain.
#' @noRd
get_chain_files <- function(.mod, extension, chain_dirs = NULL,
                            check_exists = c("no", "all", "all_or_none")) {
  check_exists <- match.arg(check_exists)
  dirs <- chain_dirs %||% get_chain_dirs(.mod)
  files <- file.path(dirs, fs::path_ext_set(basename(dirs), extension))

  if (!identical(check_exists, "no")) {
    nchains <- length(dirs)
    exist <- file.exists(files)
    nexist <- sum(exist)

    if (identical(check_exists, "all_or_none") && nexist == 0) {
      return(character(0))
    }

    if (nexist != nchains) {
      stop(glue("Missing {nchains} expected file(s):\n", .trim = FALSE),
           paste(" -", files[!exist], collapse = "\n"))
    }
  }

  return(files)
}

fread_chain_file <- function(file, select = NULL, skip = 1) {
  tibble::as_tibble(
    data.table::fread(file = file,
                      na.strings = ".",
                      data.table = FALSE,
                      verbose = FALSE,
                      select = select,
                      skip = skip))
}

#' Peek at beginning of file to determine column names.
#'
#' @param file,skip Passed to `data.table::fread()`. Only a single row is read,
#'   so `skip` must point `fread()` to the header line.
#' @noRd
fread_peek_at_columns <- function(file, skip = 1) {
  colnames(data.table::fread(file = file,
                             skip = skip,
                             nrows = 1,
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
