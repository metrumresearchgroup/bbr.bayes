## Adapted from test-install.R from the cmdstanr package

context("install_torsten")

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
      install_torsten(dir = dir, cores = 2, quiet = FALSE,
                      release_url = torsten_test_tarball_url),
      "--- Torsten",
      fixed = TRUE
    ),
    "* Finished installing Torsten",
    fixed = TRUE
  )
})

# test_that("install_torsten() errors if it times out", {
#   if (getRversion() < '3.5.0') {
#     dir <- tempdir()
#   } else {
#     dir <- tempdir(check = TRUE)
#   }
#   # with quiet=TRUE
#   expect_warning(
#     expect_message(
#       install_torsten(dir = dir, timeout = 1, quiet = TRUE,
#                       release_url = torsten_test_tarball_url),
#       "* * Installing Torsten from https://github.com",
#       fixed = TRUE
#     ),
#     "increasing the value of the 'timeout' argument and running again with 'quiet=FALSE'",
#     fixed = TRUE
#   )
#   # with quiet=FALSE
#   expect_warning(
#     expect_message(
#       install_cmdstan(dir = dir, timeout = 1, quiet = FALSE,
#                       release_url = cmdstan_test_tarball_url),
#       "* * Installing Torsten from https://github.com",
#       fixed = TRUE
#     ),
#     "Try increasing the value of the 'timeout' argument.",
#     fixed = TRUE
#   )
# })

test_that("install_torsten() errors if invalid version or URL", {
  expect_error(
    install_torsten(version = "0.89.2"),
    "Available Torsten versions do not include 0.89.2"
  )
  expect_error(
    install_torsten(release_url = "https://github.com/metrumresearchgroup/Torsten/archive/refs/tags/torsten_v0.89.2.tar.gz"),
    "Download of Torsten failed. Please check if the supplied release URL is valid."
  )
  expect_error(
    install_torsten(release_url = "https://github.com/metrumresearchgroup/Torsten/archive/refs/tags/0.89.2"),
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
      install_torsten(dir = dir, cores = 4,
                      release_url = "https://github.com/metrumresearchgroup/Torsten/archive/refs/tags/torsten_v0.89.1.tar.gz"),
      "--- Torsten",
      fixed = TRUE
    ),
    "* Finished installing Torsten",
    fixed = TRUE
  )
  expect_warning(
    expect_message(
      expect_output(
        install_torsten(dir = dir, cores = 4,
                        version = "0.89.1",
                        # the URL is intentionally invalid to test that the version has higher priority
                        release_url = "https://github.com/metrumresearchgroup/Torsten/archive/refs/tags/torsten_v0.89.1.tar.gz"),
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

test_that("Downloads respect quiet argument", {
  if (getRversion() < '3.5.0') {
    dir <- tempdir()
  } else {
    dir <- tempdir(check = TRUE)
  }
  version <- latest_torsten_release()

##  ver_msg <- "trying URL 'https://api.github.com/repos/stan-dev/cmdstan/releases/latest'"
  download_msg <- paste0("trying URL 'https://github.com/metrumresearchgroup/Torsten/archive/refs/tags/",
                         version, ".tar.gz'")

  # expect_message has trouble capturing the messages from download.file
  # so handle manually
  install_normal <- suppressWarnings(
    capture.output(install_torsten(dir = dir, quiet = FALSE),
                   type = "message")
  )
  install_quiet <- suppressWarnings(
    capture.output(install_torsten(dir = dir, quiet = TRUE),
                   type = "message")
  )

##  expect_true(any(grepl(ver_msg, install_normal, fixed = TRUE)))
  expect_true(any(grepl(download_msg, install_normal, fixed = TRUE)))

##  expect_false(any(grepl(ver_msg, install_quiet, fixed = TRUE)))
  expect_false(any(grepl(download_msg, install_quiet, fixed = TRUE)))
})

# test_that("Download failures return error message", {
#   # GHA fails on Windows old-rel here, but cannot replicate locally
#   skip_if(os_is_windows() && getRversion() < '4.2')
#
#   if (getRversion() < '3.5.0') {
#     dir <- tempdir()
#   } else {
#     dir <- tempdir(check = TRUE)
#   }
#
#   expect_error({
#     # Use an invalid proxy address to force a download failure
#     withr::with_envvar(
#       c("http_proxy"="invalid","https_proxy"="invalid"),
#       install_torsten(dir = dir)
#     )},
#     "GitHub download of release list failed with error: cannot open URL 'https://api.github.com/repos/stan-dev/cmdstan/releases/latest'")
# })

