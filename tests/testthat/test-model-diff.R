
test_that("model_diff.bbi_stan_model happy path based_on", {
  new_mod_name <- "model_diff_stan1"
  mod2 <- copy_model_from(STAN_MOD1, new_mod_name)
  on.exit(cleanup_model(mod2))
  expect_message(res_object <- model_diff(mod2),
                 "identical")
  expect_null(res_object)
})

test_that("model_diff.bbi_stan_model happy path .mod2 arg", {
  new_mod_name <- "model_diff_stan2"
  mod2 <- copy_model_from(STAN_MOD1, new_mod_name)
  on.exit(cleanup_model(mod2))
  expect_message(res_object <- model_diff(STAN_MOD1, .mod2 = mod2),
                 "identical")
  expect_null(res_object)
})

test_that("model_diff.bbi_stan_model works with other files", {
  new_mod_name <- "model_diff_stan3"
  mod2 <- copy_model_from(STAN_MOD1, new_mod_name)
  on.exit(cleanup_model(mod2))
  fs::file_copy(
    build_path_from_model(STAN_MOD1, STANARGS_SUFFIX),
    build_path_from_model(mod2, STANARGS_SUFFIX)
  )

  .test_cases <- list(
    list(file = "standata", content = "make_standata"),
    list(file = "init", content = "make_init"),
    list(file = "stanargs", content = "list")
  )

  for (.tc in .test_cases) {
    expect_message(res_object <- model_diff(mod2, .file = .tc$file),
                   "identical")
    expect_null(res_object)
  }
})
