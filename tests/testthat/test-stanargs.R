test_that("set_stanargs modifies file", {
  tdir <- local_test_dir()
  mod2 <- copy_model_from(STAN_MOD1, file.path(tdir, "stanmod"))

  file_res <- readLines(build_path_from_model(mod2, STANARGS_SUFFIX))
  expect_false(any(grepl("chains = 8", file_res)))

  set_stanargs(mod2, list(chains = 8))
  file_res <- readLines(build_path_from_model(mod2, STANARGS_SUFFIX))
  expect_true(any(grepl("chains = 8", file_res)))
})


test_that("set_stanargs catches reserved args", {
  tdir <- local_test_dir()
  mod2 <- copy_model_from(STAN_MOD1, file.path(tdir, "stanmod"))

  expect_error(
    set_stanargs(mod2, list(output_dir = "naw")),
    regexp = "Cannot add any of the following args"
  )
})

test_that("set_stanargs catches init", {
  tdir <- local_test_dir()
  mod2 <- copy_model_from(STAN_MOD1, file.path(tdir, "stanmod"))

  expect_error(
    set_stanargs(mod2, list(init = 3)),
    regexp = "Cannot pass.+initial values"
  )
})

test_that("set_stanargs checks for unknown args", {
  tdir <- local_test_dir()
  mod2 <- copy_model_from(STAN_MOD1, file.path(tdir, "stanmod"))

  expect_error(
    set_stanargs(mod2, list(youdontknowme = NULL)),
    regexp = "not accepted"
  )
})

test_that("get_stanargs returns expected list", {
  tdir <- local_test_dir()
  mod2 <- copy_model_from(STAN_MOD1, file.path(tdir, "stanmod"))

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
