test_that("set_stanargs modifies file", {
  mod2 <- copy_model_from(STAN_MOD1, STAN_MOD_ID2, .overwrite =T)
  withr::defer(cleanup_model(mod2))

  file_res <- readLines(build_path_from_model(mod2, STANARGS_SUFFIX))
  expect_false(any(grepl("chains = 8", file_res)))

  set_stanargs(mod2, list(chains = 8))
  file_res <- readLines(build_path_from_model(mod2, STANARGS_SUFFIX))
  expect_true(any(grepl("chains = 8", file_res)))
})


test_that("set_stanargs catches reserved args", {
  mod2 <- copy_model_from(STAN_MOD1, STAN_MOD_ID2, .overwrite =T)
  withr::defer(cleanup_model(mod2))

  expect_error(
    set_stanargs(mod2, list(output_dir = "naw")),
    regexp = "Cannot add any of the following args"
  )
})

test_that("set_stanargs catches init", {
  mod2 <- copy_model_from(STAN_MOD1, STAN_MOD_ID2, .overwrite =T)
  withr::defer(cleanup_model(mod2))

  expect_error(
    set_stanargs(mod2, list(init = 3)),
    regexp = "Cannot pass.+initial values"
  )
})

test_that("set_stanargs checks for unknown args", {
  mod2 <- copy_model_from(STAN_MOD1, STAN_MOD_ID2, .overwrite = TRUE)
  withr::defer(cleanup_model(mod2))

  expect_error(
    set_stanargs(mod2, list(youdontknowme = NULL)),
    regexp = "not accepted"
  )
})

test_that("get_stanargs returns expected list", {
  mod2 <- copy_model_from(STAN_MOD1, STAN_MOD_ID2, .overwrite =T)
  withr::defer(cleanup_model(mod2))

  file_res <- dget(build_path_from_model(mod2, STANARGS_SUFFIX))

  res <- get_stanargs(mod2)
  expect_true(inherits(res, "bbr_stanargs"))

  purrr::walk(names(res), ~{
    expect_equal(res[[.x]], file_res[[.x]])
  })
})

test_that("get_known_params() aborts on unknown method", {
  expect_error(get_known_params("youdontknowme"),
               "Unknown")
})
