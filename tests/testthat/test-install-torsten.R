## Adapted from test-install.R from the cmdstanr package

torsten_test_tarball_url <- Sys.getenv("TORSTEN_TEST_TARBALL_URL")
if (!nzchar(torsten_test_tarball_url)) {
  torsten_test_tarball_url <- NULL
}

test_that("install_torsten() successfully installs torsten", {
  if (getRversion() < '3.5.0') {
    dir <- tempdir()
  } else {
    dir <- tempdir(check = TRUE)
  }
  expect_message(
    expect_output(
      install_torsten(dir = dir, quiet = FALSE,
                      release_url = torsten_test_tarball_url),
      "--- Torsten",
      fixed = TRUE
    ),
    "* Finished installing Torsten",
    fixed = TRUE
  )
})

test_that("install_torsten() errors if invalid version or URL", {
  expect_error(
    install_torsten(version = "0.89.2"),
    "Available Torsten versions do not include 0.89.2"
  )
  expect_error(
    install_torsten(release_url = paste0(TORSTEN_URL_BASE, "torsten_v0.89.2.tar.gz")),
    "Download of Torsten failed. Please check if the supplied release URL is valid."
  )
  expect_error(
    install_torsten(release_url = paste0(TORSTEN_URL_BASE, "0.89.2")),
    "cmdstanr supports installing from .tar.gz archives only"
  )
})

test_that("install_torsten() works with version and release_url", {
  if (getRversion() < '3.5.0') {
    dir <- tempdir()
  } else {
    dir <- tempdir(check = TRUE)
  }

  expect_message(
    expect_output(
      install_torsten(dir = dir,
                      release_url = paste0(TORSTEN_URL_BASE, "torsten_v0.89.1.tar.gz")),
      "--- Torsten",
      fixed = TRUE
    ),
    "* Finished installing Torsten",
    fixed = TRUE
  )
  expect_warning(
    expect_message(
      expect_output(
        install_torsten(dir = dir,
                        version = "0.89.1",
                        # the URL is intentionally invalid to test that the version has higher priority
                        release_url = paste0(TORSTEN_URL_BASE, "torsten_v0.89.3.tar.gz")),
        "--- Torsten",
        fixed = TRUE
      ),
      "* Finished installing Torsten",
      fixed = TRUE
    ),
    "version and release_url shouldn't both be specified",
    fixed = TRUE
  )
  expect_true(dir.exists(file.path(dir, "torsten_v0.89.1")))
})