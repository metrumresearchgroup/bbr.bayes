
### NONMEM Bayes

test_that("nmbayes: model_diff() happy path based_on", {
  mod2 <- copy_model_from(NMBAYES_MOD1, "1100-test")
  on.exit(cleanup_model(mod2))
  res <- model_diff(mod2)
  expect_true(inherits(res, "Diff"))

  out_lines <- capture.output(print(res))
  diff_lines <- stringr::str_subset(out_lines, "<")
  expect_length(diff_lines, 2)
  expect_match(diff_lines, "PROBLEM", all = FALSE)
})

test_that("nmbayes: model_diff() happy path .mod2 arg", {
  mod2 <- copy_model_from(NMBAYES_MOD1, "1100-test")
  on.exit(cleanup_model(mod2))
  res <- model_diff(NMBAYES_MOD1, mod2)
  expect_true(inherits(res, "Diff"))

  out_lines <- capture.output(print(res))
  diff_lines <- stringr::str_subset(out_lines, "<")
  expect_length(diff_lines, 2)
  expect_match(diff_lines, "PROBLEM", all = FALSE)
})

### Stan

test_that("stan: model_diff() happy path based_on", {
  new_mod_name <- "model_diff_stan1"
  mod2 <- copy_model_from(STAN_MOD1, new_mod_name)
  on.exit(cleanup_model(mod2))
  expect_message(res_object <- model_diff(mod2),
                 "identical")
  expect_null(res_object)
})

test_that("stan: model_diff() happy path .mod2 arg", {
  new_mod_name <- "model_diff_stan2"
  mod2 <- copy_model_from(STAN_MOD1, new_mod_name)
  on.exit(cleanup_model(mod2))
  expect_message(res_object <- model_diff(STAN_MOD1, .mod2 = mod2),
                 "identical")
  expect_null(res_object)
})

test_that("stan: model_diff() works with other files", {
  new_mod_name <- "model_diff_stan3"
  mod2 <- copy_model_from(STAN_MOD1, new_mod_name)
  on.exit(cleanup_model(mod2))

  files <- c("standata", "init", "stanargs")
  for (f in files) {
    expect_message(res <- model_diff(mod2, .file = f),
                   "identical")
    expect_null(res)
  }

  expect_message(res <- model_diff(mod2, .file = "fitted-params"),
                 "Only stan_gq")
  expect_null(res)
})

test_that("stan gq: model_diff() works with other files", {
  new_mod_name <- "model_diff_stan4"
  mod2 <- copy_model_from(STAN_GQ_MOD, new_mod_name)
  on.exit(cleanup_model(mod2))

  files <- c("standata", "fitted-params", "stanargs")
  for (f in files) {
    expect_message(res <- model_diff(mod2, .file = f),
                   "identical")
    expect_null(res)
  }

  expect_message(res <- model_diff(mod2, .file = "init"),
                 "stan_gq models do not")
  expect_null(res)
})

test_that("stan: model_diff() works between regular and gq models", {
  new_mod_name <- "model_diff_stan5"
  mod2 <- copy_model_as_stan_gq(STAN_MOD3, new_mod_name)
  on.exit(cleanup_model(mod2))

  res <- model_diff(mod2, .file = "stanargs")
  expect_true(inherits(res, "Diff"))
  out_lines <- capture.output(print(res))
  expect_match(out_lines, "iter_sampling", all = FALSE)

  expect_message(res <- model_diff(mod2, .file = "standata"),
                 "identical")
  expect_null(res)

  expect_message(res <- model_diff(mod2, .file = "init"),
                 "stan_gq models do not")
  expect_null(res)

  expect_message(res <- model_diff(mod2, .file = "fitted-params"),
                 "Only stan_gq")
  expect_null(res)
})
