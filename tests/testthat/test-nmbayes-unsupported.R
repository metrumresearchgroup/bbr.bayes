test_that("cov_cor() aborts on nmbayes model", {
  expect_error(cov_cor(NMBAYES_MOD1), "not supported")
})

test_that("nm_file() aborts on nmbayes model", {
  expect_error(nm_file(NMBAYES_MOD1), "not supported")
})
