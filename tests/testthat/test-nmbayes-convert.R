
test_that("nmbayes_convert() errors on non-nonmem model", {
  expect_error(nmbayes_convert(STAN_MOD1), "bbi_nonmem_model")
})

test_that("nmbayes_convert() converts nonmem to nmbayes", {
  model_dir <- withr::local_tempdir("bbr-nmbayes-convert-")
  cat("", file = file.path(model_dir, "bbi.yaml"))
  mod <- read_model(
    system.file("model", "nonmem", "basic", "1",
                package = "bbr.bayes", mustWork = TRUE)) %>%
    copy_model_from(file.path(model_dir, "1"))
  expect_s3_class(mod, NM_MOD_CLASS)
  expect_false(inherits(mod, NMBAYES_MOD_CLASS))

  expect_no_match(readr::read_file(get_model_path(mod)),
                  "- nmbayes start -",
                  fixed = TRUE)

  mod <- nmbayes_convert(mod)
  expect_s3_class(mod, NM_MOD_CLASS)
  expect_s3_class(mod, NMBAYES_MOD_CLASS)

  expect_match(readr::read_file(get_model_path(mod)),
               "- nmbayes start -",
               fixed = TRUE)
  expect_match(readr::read_file(get_model_path(mod)),
               "- nmbayes end -",
               fixed = TRUE)

  # A nmbayes model is returned as is.
  mod_redo <- nmbayes_convert(mod)
  expect_reference(mod_redo, mod)
})

test_that("ctl_delete_section() works: empty cases", {
  cases <- list(
    list(input = list(lines = character(0),
                      prefix = "FOO"),
         expect = character(0)),
    list(input = list(lines = "",
                      prefix = "FOO"),
         expect = ""),
    list(input = list(lines = "$FOO",
                      prefix = "FOO"),
         expect = character(0)),
    list(input = list(lines = "$FOOBERT",
                      prefix = "FOO"),
         expect = character(0)),
    list(input = list(lines = "  $FOO",
                      prefix = "FOO"),
         expect = character(0)),
    list(input = list(lines = "$FOO",
                      prefix = c("FOO", "BAR")),
         expect = character(0)),
    list(input = list(lines = c("$FOO", "$BAR"),
                      prefix = c("FOO", "BAR")),
         expect = character(0)),
    list(input = list(lines = c("$FOO", "foo body"),
                      prefix = "FOO"),
         expect = character(0))
  )
  for (case in cases) {
    expect_identical(do.call(ctl_delete_section, !!case$input),
                     !!case$expect)
  }
})

test_that("ctl_delete_section() works: non-matches", {
  cases <- list(
    list(input = list(lines = "$FOO",
                      prefix = "foo"),
         expect = "$FOO"),
    list(input = list(lines = "a $FOO",
                      prefix = "FOO"),
         expect = "a $FOO"),
    list(input = list(lines = "FOO but not section",
                      prefix = "FOO"),
         expect = "FOO but not section"),
    list(input = list(lines = "$FOO",
                      prefix = "FO."),
         expect = "$FOO")
  )
  for (case in cases) {
    expect_identical(do.call(ctl_delete_section, !!case$input),
                     !!case$expect)
  }
})

test_that("ctl_delete_section() works: match found", {
  cases <- list(
    list(input = list(lines = c("$FOO", "$NEXT"),
                      prefix = "FOO"),
         expect = "$NEXT"),
    list(input = list(lines = c("pre", "$FOO", "$NEXT"),
                      prefix = "FOO"),
         expect =  c("pre", "$NEXT")),
    list(input = list(lines = c("pre", "amble",
                                "$FOO", "foo body1", "foo body2",
                                "$NEXT", "next body1", "next body2"),
                      prefix = "FOO"),
         expect =  c("pre", "amble",
                     "$NEXT", "next body1", "next body2")),
    list(input = list(lines = c("$PRE",
                                "$FOO", "foo body1", "foo body2",
                                "$NEXT", "next body1", "next body2"),
                      prefix = "FOO"),
         expect =  c("$PRE",
                     "$NEXT", "next body1", "next body2")),
    list(input = list(lines = c("$PRE",
                                "$FOO", "foo body1", "foo body2",
                                "$NEXT", "next body1", "next body2"),
                      prefix = c("FOO", "PRE")),
         expect =  c("$NEXT", "next body1", "next body2"))
  )
  for (case in cases) {
    expect_identical(do.call(ctl_delete_section, !!case$input),
                     !!case$expect)
  }
})
