skip_if_no_cmdstan()

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

test_that("find_stan_gq_children() maps stan to stan_gq models", {
  expect_identical(find_stan_gq_children(STAN_MOD1), list())
  expect_identical(find_stan_gq_children(STAN_MOD3), list(STAN_GQ_MOD))

  tdir <- local_test_dir()

  expect_warning(
    res <- find_stan_gq_children(STAN_MOD3, .base_dir = tdir),
    # From bbr:::find_models()
    "no valid model yaml files",
    ignore.case = TRUE
  )
  expect_identical(res, list())

  m_gq1 <- copy_model_from(STAN_GQ_MOD, file.path(tdir, "m_gq1"))
  res <- find_stan_gq_children(STAN_MOD3, .base_dir = tdir)
  expect_length(res, 1)
  # Do the detailed comparison once. For later checks, we'll just look at
  # ABS_MOD_PATH.
  expect_setequal(names(m_gq1), names(res[[1]]))
  for (n in names(m_gq1)) {
    expect_identical(res[[1]][[n]], m_gq1[[n]])
  }

  m_stan1 <- new_model(file.path(tdir, "m_stan1"), .model_type = "stan")
  m_gq2 <- copy_model_from(m_gq1, "m_gq2")

  res <- find_stan_gq_children(STAN_MOD3, .base_dir = tdir)
  expect_length(res, 2)
  expect_setequal(c(m_gq1[[ABS_MOD_PATH]], m_gq2[[ABS_MOD_PATH]]),
                  purrr::map(res, ABS_MOD_PATH))

  m_gq2 <- add_stan_gq_parent(m_gq2, STAN_MOD1[[ABS_MOD_PATH]])
  res <- find_stan_gq_children(STAN_MOD1, .base_dir = tdir)
  expect_length(res, 1)
  expect_identical(m_gq2[[ABS_MOD_PATH]],
                   res[[1]][[ABS_MOD_PATH]])

  subdir <- file.path(tdir, "subdir")
  fs::dir_create(subdir)
  m_gq3 <- copy_model_from(m_gq2, file.path(subdir, "m_gq3"))

  res <- find_stan_gq_children(STAN_MOD1, .base_dir = tdir)
  expect_length(res, 1)
  expect_identical(m_gq2[[ABS_MOD_PATH]],
                   res[[1]][[ABS_MOD_PATH]])

  res <- find_stan_gq_children(STAN_MOD1, .base_dir = tdir, .recurse = TRUE)
  expect_length(res, 2)
  expect_setequal(c(m_gq2[[ABS_MOD_PATH]], m_gq3[[ABS_MOD_PATH]]),
                  purrr::map(res, ABS_MOD_PATH))
})

test_that("find_stan_gq_children() checks for bbi_stan_model object", {
  expect_error(find_stan_gq_children(STAN_GQ_MOD),
               "must be model_type=stan")
  expect_error(find_stan_gq_children(NMBAYES_MOD1),
               "bbi_stan_model")
})
