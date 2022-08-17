
test_that("copy_from_model.bbi_stan_model creates accurate copy", {
  mod_name <- "testmod_copy_stan1"
  new_tags <- c("new tag 1", "new tag 2")
  new_mod <- copy_model_from(STAN_MOD1, mod_name, .inherit_tags = TRUE, .add_tags = new_tags)
  on.exit(cleanup_model(new_mod))

  # check that everything is copied through in the object
  expect_true(inherits(new_mod, STAN_MOD_CLASS))
  expect_identical(new_mod[[YAML_BASED_ON]], STAN_MOD_ID)
  expect_identical(new_mod[[YAML_TAGS]], c(STAN_MOD1$tags, new_tags))
  expect_null(new_mod[[YAML_DESCRIPTION]])
})
