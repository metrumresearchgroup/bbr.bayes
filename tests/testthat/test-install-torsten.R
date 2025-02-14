## Adapted from test-install.R from the cmdstanr package

torsten_version <- "0.91.0" # version used for actual installation
torsten_test_tarball_url_default <- paste0(
  TORSTEN_URL_BASE, "v", torsten_version, ".tar.gz"
)
torsten_test_tarball_url <- Sys.getenv("TORSTEN_TEST_TARBALL_URL")
if (!nzchar(torsten_test_tarball_url)) {
  torsten_test_tarball_url <- torsten_test_tarball_url_default
}

reset_cmdstan_path <- function(envir = parent.frame()) {
  oldpath <- cmdstanr::cmdstan_path()
  withr::defer(cmdstanr::set_cmdstan_path(oldpath), envir = envir)
}

install_torsten_maybe_skip <- function(...) {
  skip_if_over_rate_limit()
  install_torsten(...)
}

get_torsten_download_url_maybe_skip <- function(...) {
  skip_if_over_rate_limit()
  get_torsten_download_url(...)
}

test_that("install_torsten() successfully installs torsten", {
  skip_long_tests("long-running install_torsten() test")
  reset_cmdstan_path()

  dir <- local_test_dir()
  withr::local_envvar(c("R_USER_DATA_DIR" = dir))

  expect_message(
    install_torsten_maybe_skip(quiet = TRUE, release_url = torsten_test_tarball_url),
    "* Finished installing Torsten",
    fixed = TRUE
  )

  outdir <- file.path(dir, "R", "torsten")
  expect_true(file.exists(outdir))
  subdirs <- list.files(outdir)
  expect_identical(
    subdirs,
    fs::path_ext_remove(fs::path_ext_remove(basename(torsten_test_tarball_url)))
  )
  expect_identical(
    cmdstanr::cmdstan_path(),
    file.path(outdir, subdirs, "cmdstan")
  )
})

test_that("install_torsten() errors if invalid version or URL", {
  reset_cmdstan_path()

  tdir <- local_test_dir()

  expect_error(
    install_torsten_maybe_skip(dir = tdir, quiet = TRUE, version = "0.89.2"),
    "Available Torsten versions do not include 0.89.2"
  )
  expect_error(
    install_torsten_maybe_skip(
      dir = tdir, quiet = TRUE,
      release_url = paste0(TORSTEN_URL_BASE, "torsten_v0.89.2.tar.gz")
    ),
    "Download of Torsten failed. Please check if the supplied release URL is valid."
  )
  expect_error(
    install_torsten_maybe_skip(
      dir = tdir, quiet = TRUE,
      release_url = paste0(TORSTEN_URL_BASE, "0.89.2")
    ),
    "cmdstanr supports installing from .tar.gz archives only"
  )
})

test_that("install_torsten() overwrite check works", {
  reset_cmdstan_path()

  tdir <- local_test_dir()
  # Use a bogus URL to avoid the full install.
  url <- paste0(TORSTEN_URL_BASE, "torsten_v0.89.2.tar.gz")

  fs::dir_create(file.path(tdir), "torsten_v0.89.2")
  expect_warning(
    install_torsten_maybe_skip(dir = tdir, release_url = url, quiet = TRUE),
    "installation already exists"
  )
  expect_error(
    expect_message(
      install_torsten_maybe_skip(
        dir = tdir, release_url = url, quiet = TRUE,
        overwrite = TRUE
      ),
      "Removing the existing installation",
      fixed = TRUE
    ),
    "Download of Torsten failed",
    fixed = TRUE
  )
})

test_that("install_torsten() works with version and release_url", {
  expect_identical(
    get_torsten_download_url_maybe_skip(version = torsten_version, release_url = NULL),
    torsten_test_tarball_url_default
  )

  expect_identical(
    get_torsten_download_url_maybe_skip(
      version = paste0("torsten_v", torsten_version),
      release_url = NULL
    ),
    torsten_test_tarball_url_default
  )

  expect_warning(
    res <- get_torsten_download_url_maybe_skip(
      version = torsten_version,
      # the URL is intentionally invalid to test that the version has higher priority
      release_url = paste0(TORSTEN_URL_BASE, "torsten_v0.89.3.tar.gz")
    ),
    "version and release_url shouldn't both be specified",
    fixed = TRUE
  )
  expect_identical(res, torsten_test_tarball_url_default)

  url_to_version <- function(url) {
    m <- regexec("([0-9.]+)\\.tar\\.gz$", url)[[1]]
    if (length(m) != 2) {
      stop("URL does not specify version: ", url)
    }
    v <- substr(url, m[2], m[2] + attr(m, "match.length")[2] - 1)
    return(numeric_version(v))
  }

  v_latest <- url_to_version(
    get_torsten_download_url_maybe_skip(version = NULL, release_url = NULL)
  )
  # Ideally the torsten_test_tarball_url_default version would not be the latest
  # version so that get_torsten_download_url() and
  # get_torsten_download_url(version = N) would return different results (i.e.
  # we could use ">" rather than ">=" below). However, at the moment (2025-02),
  # the latest version is 0.91.0 and earlier ones fail to build on ubuntu-24.04.
  expect_true(v_latest >= url_to_version(torsten_test_tarball_url_default))
})

test_that("expand_torsten_version() works", {
  for (v in c("torsten_v1.2.3", "torsten_1.2.3", "v1.2.3", "1.2.3")) {
    expect_identical(
      expand_torsten_version(!!v),
      c(
        "torsten_v1.2.3",
        "torsten_1.2.3",
        "v1.2.3",
        "1.2.3"
      )
    )
  }
  for (v in c("torsten_v1.2.3rc0", "torsten_1.2.3rc0", "v1.2.3rc0", "1.2.3rc0")) {
    expect_identical(
      expand_torsten_version(!!v),
      c(
        "torsten_v1.2.3rc0",
        "torsten_1.2.3rc0",
        "v1.2.3rc0",
        "1.2.3rc0"
      )
    )
  }
})
