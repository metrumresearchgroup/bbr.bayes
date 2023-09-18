## Adapted from test-install.R from the cmdstanr package

skip_if_over_rate_limit()

torsten_test_tarball_url_default <- paste0(TORSTEN_URL_BASE, "torsten_v0.89.1.tar.gz")
torsten_test_tarball_url <- Sys.getenv("TORSTEN_TEST_TARBALL_URL")
if (!nzchar(torsten_test_tarball_url)) {
  torsten_test_tarball_url <- torsten_test_tarball_url_default
}

test_that("install_torsten() successfully installs torsten", {
  dir <- local_test_dir()
  withr::local_envvar(c("R_USER_DATA_DIR" = dir))

  expect_message(
    install_torsten(quiet = TRUE, release_url = torsten_test_tarball_url),
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
  tdir <- local_test_dir()

  expect_error(
    install_torsten(dir = tdir, quiet = TRUE, version = "0.89.2"),
    "Available Torsten versions do not include 0.89.2"
  )
  expect_error(
    install_torsten(
      dir = tdir, quiet = TRUE,
      release_url = paste0(TORSTEN_URL_BASE, "torsten_v0.89.2.tar.gz")
    ),
    "Download of Torsten failed. Please check if the supplied release URL is valid."
  )
  expect_error(
    install_torsten(
      dir = tdir, quiet = TRUE,
      release_url = paste0(TORSTEN_URL_BASE, "0.89.2")
    ),
    "cmdstanr supports installing from .tar.gz archives only"
  )
  expect_error(
    install_torsten(dir = tdir, quiet = TRUE, version = "0"),
    "matches multiple"
  )
})

test_that("install_torsten() overwrite check works", {
  tdir <- local_test_dir()
  # Use a bogus URL to avoid the full install.
  url <- paste0(TORSTEN_URL_BASE, "torsten_v0.89.2.tar.gz")

  fs::dir_create(file.path(tdir), "torsten_v0.89.2")
  expect_warning(
    install_torsten(dir = tdir, release_url = url, quiet = TRUE),
    "installation already exists"
  )
  expect_error(
    expect_message(
      install_torsten(
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
    get_torsten_download_url(version = "0.89.1", release_url = NULL),
    torsten_test_tarball_url_default
  )

  expect_identical(
    get_torsten_download_url(version = "torsten_v0.89.1", release_url = NULL),
    torsten_test_tarball_url_default
  )

  expect_warning(
    res <- get_torsten_download_url(
      version = "0.89.1",
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
    get_torsten_download_url(version = NULL, release_url = NULL)
  )
  expect_true(v_latest > url_to_version(torsten_test_tarball_url_default))
})
