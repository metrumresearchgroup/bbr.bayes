### tail_lst

tail_lst_q <- purrr::quietly(tail_lst)

test_that("tail_lst() works: default arguments", {
  qres <- tail_lst_q(NMBAYES_MOD1)

  expect_null(qres$result)

  msgs <- qres$messages
  expect_match(msgs, "Chain 1", fixed = TRUE, all = FALSE)
  expect_match(msgs, "Chain 2", fixed = TRUE, all = FALSE)

  out <- qres$output
  expect_match(out, "...", fixed = TRUE, all = FALSE)
  expect_match(out, "Stop", fixed = TRUE, all = FALSE)

  expect_length(qres$warnings, 0)
})

test_that("tail_lst() works: non-default arguments", {
  qres_no_print <- tail_lst_q(
    NMBAYES_MOD1,
    .head = 1, .tail = 1, .return = TRUE, .print = FALSE
  )
  expect_identical(
    purrr::map_int(qres_no_print$result, length),
    c(3L, 3L) # +1 for "..." in middle.
  )
  expect_length(qres_no_print$messages, 0)
  expect_false(nzchar(qres_no_print$output))
  expect_length(qres_no_print$warnings, 0)

  qres_print <- tail_lst_q(
    NMBAYES_MOD1,
    .head = 1, .tail = 1, .return = TRUE, .print = TRUE
  )
  expect_identical(qres_print$result, qres_no_print$result)
  expect_match(qres_print$messages, "Chain 1", fixed = TRUE, all = FALSE)
  expect_match(qres_print$output, "...", fixed = TRUE, all = FALSE)
})

test_that("tail_lst.character() works with nmbayes models", {
  res <- tail_lst(NMBAYES_MOD1_PATH, .return = TRUE, .print = FALSE)
  expect_length(res, 2)
  expect_identical(
    res,
    tail_lst(NMBAYES_MOD1, .return = TRUE, .print = FALSE)
  )
})

test_that("tail_lst() aborts: no top-level output directory", {
  tdir <- local_test_dir()
  cat("", file = file.path(tdir, "bbi.yaml"))
  mod <- copy_model_from(NMBAYES_MOD1, file.path(tdir, get_model_id(NMBAYES_MOD1)))
  expect_error(tail_lst(mod), "does not exist")
})

test_that("tail_lst() aborts: missing *.lst file", {
  local_dummy_nmbayes("foo")

  cat("abc\ndef\n", file = file.path("foo", "foo-2", "foo-2.lst"))
  mod <- read_model("foo")
  # A submodel error is propagated, but only after processing all chains.
  expect_error(
    expect_output(tail_lst(mod), "abc", fixed = TRUE),
    "foo-1.lst",
    fixed = TRUE
  )
})

### tail_output

tail_output_q <- purrr::quietly(tail_output)

test_that("tail_output() works: default arguments", {
  local_dummy_nmbayes("foo")

  cat("abc\ndef\n", file = file.path("foo", "foo-1", "OUTPUT"))
  cat("ghi\njkl\n", file = file.path("foo", "foo-2", "OUTPUT"))

  qres <- tail_output_q(read_model("foo"))

  expect_null(qres$result)

  msgs <- qres$messages
  expect_match(msgs, "Chain 1", fixed = TRUE, all = FALSE)
  expect_match(msgs, "Chain 2", fixed = TRUE, all = FALSE)

  out <- qres$output
  expect_match(out, "abc\ndef", fixed = TRUE, all = FALSE)
  expect_match(out, "ghi\njkl", fixed = TRUE, all = FALSE)

  expect_length(qres$warnings, 0)
})

test_that("tail_output() works: non-default arguments", {
  local_dummy_nmbayes("foo")

  cat("abc\ndef\n", file = file.path("foo", "foo-1", "OUTPUT"))
  cat("ghi\njkl\n", file = file.path("foo", "foo-2", "OUTPUT"))

  qres_no_print <- tail_output_q(
    read_model("foo"),
    .head = 1, .tail = 1, .return = TRUE, .print = FALSE
  )

  expect_identical(
    qres_no_print$result,
    list(c("abc", "def"), c("ghi", "jkl"))
  )

  expect_length(qres_no_print$messages, 0)
  expect_false(nzchar(qres_no_print$output))
  expect_length(qres_no_print$warnings, 0)

  qres_print <- tail_output_q(
    read_model("foo"),
    .head = 1, .tail = 1, .return = TRUE, .print = TRUE
  )
  expect_identical(qres_print$result, qres_no_print$result)
  expect_match(qres_print$messages, "Chain 1", fixed = TRUE, all = FALSE)
  expect_match(qres_print$output, "abc", fixed = TRUE, all = FALSE)
})

test_that("tail_output.character() works with nmbayes models", {
  res <- tail_lst(NMBAYES_MOD1_PATH, .return = TRUE, .print = FALSE)
  expect_length(res, 2)
  expect_identical(
    res,
    tail_lst(NMBAYES_MOD1, .return = TRUE, .print = FALSE)
  )
})

test_that("tail_output() works: .print=TRUE .return=FALSE, finished", {
  qres <- tail_output_q(NMBAYES_MOD1)

  expect_null(qres$result)

  msgs <- qres$messages
  expect_match(msgs, "Chain 1", fixed = TRUE, all = FALSE)
  expect_match(msgs, "Chain 2", fixed = TRUE, all = FALSE)
  expect_match(msgs, "already finished", fixed = TRUE, all = FALSE)

  expect_false(nzchar(qres$output))
  expect_length(qres$warnings, 0)
})

test_that("tail_output() works: .print=FALSE .return=TRUE, finished", {
  qres <- tail_output_q(NMBAYES_MOD1, .print = FALSE, .return = TRUE)

  expect_identical(qres$result, list(NULL, NULL))
  msgs <- qres$messages
  expect_no_match(msgs, "Chain 1", fixed = TRUE)
  expect_no_match(msgs, "Chain 2", fixed = TRUE)
  expect_false(nzchar(qres$output))
  expect_length(qres$warnings, 0)
})

test_that("tail_output() works: one finished", {
  local_dummy_nmbayes("foo")
  cat("abc\ndef\n", file = file.path("foo", "foo-2", "OUTPUT"))

  qres <- tail_output_q(read_model("foo"), .return = TRUE, .print = FALSE)
  expect_identical(qres$result, list(NULL, c("abc", "def")))
})
