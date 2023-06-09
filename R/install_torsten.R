install_torsten <- function(dir = NULL,
                            cores = getOption("mc.cores", 2),
                            quiet = FALSE,
##                            overwrite = FALSE,
                            timeout = 1200,
                            version = NULL,
                            release_url = NULL,
                            cpp_options = list(),
                            check_toolchain = TRUE,
                            mpi_path = NULL
) {
  require(cmdstanr)
  if (check_toolchain) {
    cmdstanr::check_cmdstan_toolchain(fix = FALSE, quiet = quiet)
  }
  make_local_msg <- NULL
  if (!is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
    current_make_local_contents <- cmdstanr::cmdstan_make_local()
    if (length(current_make_local_contents) > 0) {
      old_cmdstan_path <- cmdstanr::cmdstan_path()
      make_local_msg <- paste0("cmdstan_make_local(cpp_options = cmdstan_make_local(dir = \"", cmdstan_path(), "\"))")
    }
  }
  if (is.null(dir)) {
    dir <- cmdstanr::cmdstan_default_install_path()
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  if (!is.null(version)) {
    if (!is.null(release_url)) {
      warning("version and release_url shouldn't both be specified!",
              "\nrelease_url will be ignored.", call. = FALSE)
    }

    release_list <- get_torsten_release_list()
    release <- release_list[grep(version, release_list)]
    if(length(release) < 1){
      stop("Available Torsten versions do not include", version, call. = FALSE)
    }
    release_url <- paste0("https://github.com/metrumresearchgroup/Torsten/archive/refs/tags/",
                          release, ".tar.gz")
  }
  if (!is.null(release_url)) {
    if (!endsWith(release_url, ".tar.gz")) {
      stop(release_url, " is not a .tar.gz archive!",
           "cmdstanr supports installing from .tar.gz archives only.", call. = FALSE)
    }
    message("* Installing Torsten from ", release_url)
    # download_url <- release_url
    # split_url <- strsplit(release_url, "/")
    # tar_name <- utils::tail(split_url[[1]], n = 1)
    # cmdstan_ver <- substr(tar_name, 0, nchar(tar_name) - 7)
    # tar_gz_file <- paste0(cmdstan_ver, ".tar.gz")
    # dir_cmdstan <- file.path(dir, cmdstan_ver)
    # dest_file <- file.path(dir, tar_gz_file)
  } else {
    ver <- latest_torsten_release()
    message("* Latest Torsten release is ", ver)
    release_url <- paste0("https://github.com/metrumresearchgroup/Torsten/archive/refs/tags/",
                          ver, ".tar.gz")
    # cmdstan_ver <- paste0("cmdstan-", ver, cmdstan_arch_suffix(ver))
    # tar_gz_file <- paste0(cmdstan_ver, ".tar.gz")
    # dir_cmdstan <- file.path(dir, cmdstan_ver)
    # message("* Installing CmdStan v", ver, " in ", dir_cmdstan)
    # message("* Downloading ", tar_gz_file, " from GitHub...")
    # download_url <- github_download_url(ver)
    # dest_file <- file.path(dir, tar_gz_file)
  }
  dir_torsten <- file.path(dir, substr(basename(release_url), 1, nchar(basename(release_url)) - 7))
  dir.create(dir_torsten, recursive = TRUE)
  dest_file <- file.path(dir_torsten, basename(release_url))
  dir_cmdstan <- file.path(dir_torsten, "cmdstan")
##  if (!check_install_dir(dir_cmdstan, overwrite)) {
##    return(invisible(NULL))
##  }
  ## Reset timeout for download. The 60 s default is not enough.
  options(timeout = max(300, getOption("timeout")))
  tar_downloaded <- cmdstanr:::download_with_retries(release_url, dest_file)
  if (!tar_downloaded) {
    if (!is.null(version)) {
      stop("Download of Torsten failed. Please check if the supplied version number is valid.", call. = FALSE)
    }
    if (!is.null(release_url)) {
      stop("Download of Torsten failed. Please check if the supplied release URL is valid.", call. = FALSE)
    }
    stop("Download of Torsten failed. Please try again.", call. = FALSE)
  }
  message("* Download complete")

  message("* Unpacking archive...")
  untar_rc <- utils::untar(
    dest_file,
    exdir = dir_torsten,
    extras = "--strip-components 1"
  )
  if (untar_rc != 0) {
    stop("Problem extracting tarball. Exited with return code: ", untar_rc, call. = FALSE)
  }
  file.remove(dest_file)
  cmdstanr::cmdstan_make_local(dir = dir_cmdstan, cpp_options = cpp_options, append = TRUE)
  # Setting up native M1 compilation of CmdStan and its downstream libraries
  if (cmdstanr:::is_rosetta2()) {
    cmdstanr::cmdstan_make_local(
      dir = dir_cmdstan,
      cpp_options = list(
        CXX = "arch -arch arm64e clang++"
      ),
      append = TRUE
    )
  }

  message("* Building CmdStan binaries...")
  build_log <- cmdstanr:::build_cmdstan(dir_cmdstan, cores, quiet, timeout)
  if (!cmdstanr:::build_status_ok(build_log, quiet = quiet)) {
    return(invisible(build_log))
  }

  set_cmdstan_path(path = file.path(dir_cmdstan))
  message("Testing installation by attempting to compile an example model.\n")
  model <- cmdstan_model(file.path(dir_torsten, "example-models", "pk2cpt", "pk2cpt.stan"),
                         force_recompile = TRUE)

  message("* Finished installing Torsten to ", dir_torsten, "\n")
  if (!is.null(make_local_msg) && old_cmdstan_path != cmdstan_path()) {
    message(
      "\nThe previous installation of CmdStan had a non-empty make/local file.\n",
      "If you wish to copy the file to the new installation, run the following commands:\n",
      "\n",
      make_local_msg,
      "\nrebuild_cmdstan(cores = ...)"
    )
  }
}
