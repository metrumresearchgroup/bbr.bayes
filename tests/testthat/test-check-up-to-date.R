
ALL_GOOD <- c(model = TRUE, data = TRUE)
MODEL_BAD <- c(model = FALSE, data = TRUE)
DATA_BAD <- c(model = TRUE, data = FALSE)

### NONMEM Bayes

test_that("nmbayes: check_up_to_date() happy path", {
  expect_equal(check_up_to_date(NMBAYES_MOD1), ALL_GOOD)
})

test_that("nmbayes: check_up_to_date with mismatched model", {
  perturb_file(get_model_path(NMBAYES_MOD1))
  expect_message(
    res <- check_up_to_date(NMBAYES_MOD1),
    regexp = "The following files have changed.+ctl")
  expect_equal(res, MODEL_BAD)
})

test_that("nmbayes: check_up_to_date() with mismatched data", {
  perturb_file(get_data_path(NMBAYES_MOD1))
  expect_message(
    res <- check_up_to_date(NMBAYES_MOD1),
    regexp = "The following files have changed.+csv"
  )
  expect_equal(res, DATA_BAD)
})

### Stan

test_that("stan: check_up_to_date() happy path", {
  expect_equal(check_up_to_date(STAN_MOD1), ALL_GOOD)
})

test_that("stan: check_up_to_date() with mismatched model", {
  perturb_file(build_path_from_model(STAN_MOD1, STANMOD_SUFFIX))
  expect_message(
    res <- check_up_to_date(STAN_MOD1),
    regexp = "The following files have changed.+\\.stan"
  )
  expect_equal(res, MODEL_BAD)
})

test_that("stan: check_up_to_date() with mismatched data .build_data=TRUE", {
  perturb_file(
    system.file("extdata", "fxa.data.csv", package = "bbr.bayes"),
    txt = paste(rep(99, 8), collapse = ",")
  )

  expect_message(
    res <- check_up_to_date(STAN_MOD1),
    regexp = "The following files have changed.+standata\\.R.+different results"
  )
  expect_equal(res, DATA_BAD)
})

test_that("stan: check_up_to_date() with mismatched data .build_data=F", {
  perturb_file(build_path_from_model(STAN_MOD1, STANDATA_JSON_SUFFIX))
  expect_message(
    res <- check_up_to_date(STAN_MOD1, .build_data = FALSE),
    regexp = "The following files have changed.+standata\\.json"
  )
  expect_equal(res, DATA_BAD)
})
