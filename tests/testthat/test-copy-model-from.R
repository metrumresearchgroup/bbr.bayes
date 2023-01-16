
test_that("copy_from_model() creates accurate copy", {
  cases <- list(list(name = "testmod_copy_nmbayes1",
                     mod = NMBAYES_MOD1,
                     expected_class = NMBAYES_MOD_CLASS,
                     expected_based_on = NMBAYES_MOD_ID),
                list(name = "testmod_copy_stan1",
                     mod = STAN_MOD1,
                     expected_class = STAN_MOD_CLASS,
                     expected_based_on = STAN_MOD_ID))

  mods <- vector(mode = "list", length = length(cases))
  on.exit(purrr::walk(mods, cleanup_model))

  for (i in seq_along(cases)) {
    case <- cases[[i]]
    new_tags <- c("new tag 1", "new tag 2")
    new_mod <- copy_model_from(case$mod, case$name,
                               .inherit_tags = TRUE,
                               .add_tags = new_tags)
    mods[[i]] <- new_mod

    # check that everything is copied through in the object
    expect_true(inherits(new_mod, !!case$expected_class))
    expect_identical(new_mod[[YAML_BASED_ON]], !!case$expected_based_on)
    expect_identical(new_mod[[YAML_TAGS]], c(!!!case$mod$tags, new_tags))
    expect_null(new_mod[[YAML_DESCRIPTION]])
  }
})

test_that("stan: copy_model_from() handles .new_model=NULL", {
  tdir <- local_test_dir()
  m1 <- copy_model_from(STAN_MOD1, file.path(tdir, "001"))
  m2 <- copy_model_from(m1)
  expect_identical(get_model_id(m2), "002")
})
