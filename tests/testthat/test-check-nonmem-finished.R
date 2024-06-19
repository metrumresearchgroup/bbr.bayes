test_that("check_nonmem_finished(): finished nmbayes model", {
  expect_true(check_nonmem_finished(NMBAYES_MOD1))
})

test_that("check_nonmem_finished(): no output directory", {
  tdir <- local_test_dir()
  mod <- copy_model_from(NMBAYES_MOD1, file.path(tdir, "foo"))
  expect_true(check_nonmem_finished(mod))
})

test_that("check_nonmem_finished() works: sequence", {
  local_dummy_nmbayes("foo")
  mod <- read_model("foo")

  expect_false(check_nonmem_finished(mod))

  # For plain NONMEM models under bbr v1.10.0 and earlier,
  # check_nonmem_finished() looks for "Stop Time" in the lst file to decide if a
  # model is finished. After that, the existence of bbi_config.json is used
  # instead. Once the minimum bbr version is at least 1.11.0, the lst setup
  # below can be dropped.

  cat("", file = file.path("foo", "foo-1", "foo-1.lst"))
  expect_false(check_nonmem_finished(mod))

  cat("", file = file.path("foo", "foo-2", "foo-2.lst"))
  expect_false(check_nonmem_finished(mod))

  cat("Stop Time", file = file.path("foo", "foo-2", "foo-2.lst"))
  cat("", file = file.path("foo", "foo-2", "bbi_config.json"))
  expect_false(check_nonmem_finished(mod))

  cat("Stop Time", file = file.path("foo", "foo-1", "foo-1.lst"))
  cat("", file = file.path("foo", "foo-1", "bbi_config.json"))
  expect_true(check_nonmem_finished(mod))
})
