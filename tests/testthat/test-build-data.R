context("building data objects for modeling")

test_that("build_data.bbi_stan_model returns correct list", {
  for (mod in list(STAN_MOD1, STAN_GQ_MOD)) {
    res <- suppressMessages(build_data(mod))
    ref <- jsonlite::fromJSON(get_data_path(mod))
    expect_equal(res, ref)
  }
})

test_that("build_data.bbi_stan_model write to disk", {
  for (mod in list(STAN_MOD1, STAN_GQ_MOD)) {
    if (identical(mod, STAN_MOD1)) {
      skip_if_jsonlite_prec_change()
    }

    tmp_path <- tempfile()

    # check for json error first
    expect_error(
      suppressMessages(build_data(mod, .out_path = tmp_path)),
      regexp = "a JSON file will be written"
    )

    # now check for real
    tmp_path <- fs::path_ext_set(tmp_path, ".json")
    suppressMessages(build_data(mod, .out_path = tmp_path))
    on.exit(fs::file_delete(tmp_path))

    expect_equal(
      as.character(tools::md5sum(tmp_path)),
      as.character(tools::md5sum(get_data_path(mod)))
    )
  }
})

test_that("build_data.bbi_stan_model errors with flawed -standata.R", {
  for (mod in list(STAN_MOD1, STAN_GQ_MOD)) {
    tdir <- local_test_dir()
    new_mod <- copy_model_from(STAN_MOD1, file.path(tdir, "new"))

    if (identical(mod, STAN_MOD1)) {
      # fails because it can't find the relative path to the data from the temp dir
      #
      # This is relevant for STAN_GQ_MOD because its data isn't read in from an
      # external file.
      expect_error(
        build_data(new_mod),
        regexp = "Calling `make_standata.+FAILED.+fxa.data.csv' does not exist"
      )
    }

    # replace -standata.R with non-working code
    writeLines(
      "naw <- function() {'naw'",
      build_path_from_model(new_mod, STANDATA_R_SUFFIX)
    )
    expect_error(
      build_data(new_mod),
      regexp = "Loading.+FAILED.+unexpected end of input"
    )

    # replace -standata.R with function that returns wrong type
    writeLines(
      "make_standata <- function(.dir) 3",
      build_path_from_model(new_mod, STANDATA_R_SUFFIX)
    )
    expect_error(
      build_data(new_mod),
      regexp = "`make_standata\\(\\)` was expected to be list"
    )

    # replace -standata.R with dummy function
    writeLines(
      "naw <- function() {'naw'}",
      build_path_from_model(new_mod, STANDATA_R_SUFFIX)
    )
    expect_error(
      build_data(new_mod),
      regexp = "must contain a function called `make_standata`"
    )
  }
})
