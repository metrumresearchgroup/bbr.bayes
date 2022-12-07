
test_that("get_chain_files() returns files chain subdirs", {
  exts <- get_chain_files(NMBAYES_MOD1, ".ext")

  expect_length(exts, 2)
  expect_true(all(fs::path_has_parent(exts, NMBAYES_ABS_MODEL_DIR)))
  expect_setequal(basename(exts), c("1100_1.ext", "1100_2.ext"))

  # It doesn't matter if "." is included.
  expect_identical(exts, get_chain_files(NMBAYES_MOD1, "ext"))

  expect_setequal(basename(get_chain_files(NMBAYES_MOD1, ".phi")),
                  c("1100_1.phi", "1100_2.phi"))
})
