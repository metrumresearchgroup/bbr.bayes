
test_that("gq_parent helpers abort if model type is not stan_gq", {
  mod <- bbr::new_model(local_test_dir(), .model_type = "stan")
  expect_error(get_stan_gq_parent(mod), "bbi_stan_gq_model")

  fns <- list(add_stan_gq_parent,
              remove_stan_gq_parent,
              replace_all_stan_gq_parent)
  for (fn in fns) {
    expect_error(fn(mod, "/dev/null"), "bbi_stan_gq_model")
  }
})

test_that("gq_parent helpers enable setting and getting gq_parent field", {
  tdir <- local_test_dir()
  mod_parent1 <- new_model(file.path(tdir, "foo"),
                           .model_type = "stan")
  # Another parent in a different directory.
  mod_parent2 <- new_model(file.path(local_test_dir(), "bar"),
                           .model_type = "stan")

  mod_gq <- new_model(file.path(tdir, "baz"), .model_type = "stan_gq")

  expect_null(get_stan_gq_parent(mod_gq))

  mod_gq <- add_stan_gq_parent(mod_gq, "foo")
  expect_identical(get_stan_gq_parent(mod_gq),
                   mod_parent1[[ABS_MOD_PATH]])

  mod_gq <- add_stan_gq_parent(mod_gq, mod_parent2[[ABS_MOD_PATH]])
  expect_identical(get_stan_gq_parent(mod_gq),
                   c(mod_parent1[[ABS_MOD_PATH]],
                     mod_parent2[[ABS_MOD_PATH]]))

  mod_gq <- remove_stan_gq_parent(mod_gq, mod_parent2[[ABS_MOD_PATH]])
  expect_identical(get_stan_gq_parent(mod_gq),
                   mod_parent1[[ABS_MOD_PATH]])

  mod_gq <- add_stan_gq_parent(mod_gq, mod_parent2[[ABS_MOD_PATH]])
  parents <- get_stan_gq_parent(mod_gq)
  mod_gq <- remove_stan_gq_parent(mod_gq, parents)
  expect_null(get_stan_gq_parent(mod_gq))

  mod_gq <- add_stan_gq_parent(mod_gq, mod_parent1[[ABS_MOD_PATH]])
  expect_identical(get_stan_gq_parent(mod_gq),
                   mod_parent1[[ABS_MOD_PATH]])

  mod_gq <- replace_all_stan_gq_parent(mod_gq, mod_parent2[[ABS_MOD_PATH]])
  expect_identical(get_stan_gq_parent(mod_gq),
                   mod_parent2[[ABS_MOD_PATH]])

  mod_gq <- replace_all_stan_gq_parent(mod_gq, NULL)
  expect_null(get_stan_gq_parent(mod_gq))

  mod_gq <- add_stan_gq_parent(mod_gq, c("foo", mod_parent2[[ABS_MOD_PATH]]))
  expect_identical(get_stan_gq_parent(mod_gq),
                   c(mod_parent1[[ABS_MOD_PATH]],
                     mod_parent2[[ABS_MOD_PATH]]))
})
