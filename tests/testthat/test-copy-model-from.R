
test_that("copy_from_model() creates accurate copy", {
  cases <- list(list(name = "testmod_copy_nmbayes1",
                     mod = NMBAYES_MOD1,
                     expected_class = NMBAYES_MOD_CLASS,
                     expected_based_on = NMBAYES_MOD_ID),
                list(name = "testmod_copy_stan1",
                     mod = STAN_MOD1,
                     expected_class = STAN_MOD_CLASS,
                     expected_based_on = STAN_MOD_ID),
                list(name = "testmod_copy_stan_gq1",
                     mod = STAN_GQ_MOD,
                     expected_class = STAN_GQ_MOD_CLASS,
                     expected_based_on = STAN_GQ_MOD_ID))

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

### NONMEM Bayes

test_that("copy_model_as_nmbayes() creates nmbayes model", {
  testthat::skip_if_not_installed("nmrec")
  tdir <- local_test_dir()
  fs::file_copy(fs::path_ext_set(NM_MOD1_PATH, ".ctl"), tdir)
  fs::file_copy(fs::path_ext_set(NM_MOD1_PATH, ".yaml"), tdir)

  check <- function(mod) {
    expect_s3_class(mod, NMBAYES_MOD_CLASS)
    expect_identical(bbr::get_based_on(mod), file.path(tdir, NM_MOD_ID))
    expect_identical(mod[[YAML_TAGS]], c(NM_MOD1$tags, "bayes"))
    ctl_file <- get_model_path(mod)
    expect_match(readLines(ctl_file), "nmbayes models require", all = FALSE)

    ctl <- nmrec::read_ctl(ctl_file)
    ests <- nmrec::select_records(ctl, "estimation")
    expect_identical(
      purrr::map_chr(ests, function(r) {
        opt <- nmrec::get_record_option(r, "method")
        if (!is.null(opt)) {
          return(opt$value)
        }
      }),
      c("CHAIN", "NUTS")
    )
  }

  parent <- read_model(file.path(tdir, NM_MOD_ID))
  parent_file <- get_model_path(parent)

  # copy_model_as_nmbayes() add message and removes existing estimation records.
  # It can handle parent control stream with one estimation record...
  m1 <- copy_model_as_nmbayes(parent, .inherit_tags = TRUE, .add_tags = "bayes")
  check(m1)
  # ... or more than one estimation record...
  cat("\n$EST METHOD=SAEM\n$EST METHOD=ITS\n",
      file = parent_file, append = TRUE)
  m2 <- copy_model_as_nmbayes(parent, .inherit_tags = TRUE, .add_tags = "bayes")
  check(m2)
  # ... or no estimation records.
  ctl_parent <- nmrec::read_ctl(parent_file)
  is_est <- purrr::map_lgl(ctl_parent$records,
                           function(r) identical(r$name, "estimation"))
  if (sum(is_est) != 3) {
    stop("Test setup is incorrect")
  }
  ctl_parent$records[is_est] <- NULL
  nmrec::write_ctl(ctl_parent, parent_file)
  m3 <- copy_model_as_nmbayes(parent, .inherit_tags = TRUE, .add_tags = "bayes")
  check(m3)
})

test_that("copy_model_as_nmbayes(): .update_model_file=FALSE disables model updates", {
  tdir <- local_test_dir()
  mod <- copy_model_as_nmbayes(NM_MOD1, file.path(tdir, "foo"),
                               .update_model_file = FALSE)
  expect_identical(readLines(get_model_path(NM_MOD1)),
                   readLines(get_model_path(mod)))
})

test_that("copy_model_as_nmbayes() keeps trailing comments", {
  tdir <- local_test_dir()
  file_parent <- file.path(tdir, "1.ctl")
  lines_parent <- c("$prob one",
                    "$EST METHOD=SAEM",
                    "  abort",
                    "; comment1",
                    "$EST METHOD=ITS",
                    "  ; comment 2")
  writeLines(lines_parent, file_parent)
  mod_parent <- new_model(file.path(tdir, "1"))
  mod <- copy_model_as_nmbayes(mod_parent)

  mod_lines <- readLines(get_model_path(mod))
  expect_identical(
    setdiff(lines_parent, mod_lines),
    c("$EST METHOD=SAEM", "  abort", "$EST METHOD=ITS")
  )
  expect_match(mod_lines, "nmbayes models require", all = FALSE)
})

test_that("copy_model_as_nmbayes() aborts if parent is nmbayes model", {
  testthat::skip_if_not_installed("nmrec")
  expect_error(copy_model_as_nmbayes(NMBAYES_MOD1),
               "already an nmbayes")
})

test_that("copy_model_as_nmbayes() aborts on non-NONMEM models", {
  testthat::skip_if_not_installed("nmrec")
  expect_error(copy_model_as_nmbayes(STAN_MOD1),
               "bbi_nonmem_model")
})

### Stan

test_that("stan: copy_model_from() handles .new_model=NULL", {
  for (mod in list(STAN_MOD1, STAN_GQ_MOD)) {
    tdir <- local_test_dir()
    m1 <- copy_model_from(mod, file.path(tdir, "001"))
    m2 <- copy_model_from(m1)
    expect_identical(get_model_id(m2), "002")
  }
})

test_that("copy_model_as_stan_gq() creates stan_gq model", {
  tdir <- local_test_dir()
  m1 <- copy_model_as_stan_gq(STAN_MOD1, file.path(tdir, "gq"))
  expect_s3_class(m1, STAN_GQ_MOD_CLASS)

  expect_identical(get_stan_gq_parent(m1),
                   STAN_MOD1[[ABS_MOD_PATH]])

  should_exist <- c(STANMOD_SUFFIX,
                    STANDATA_R_SUFFIX,
                    STAN_FITTED_PARAMS_SUFFIX)
  for (ext in should_exist) {
    checkmate::expect_file_exists(build_path_from_model(m1, ext))
  }

  should_not_exist <- c(STANINIT_SUFFIX,
                        STANDATA_JSON_SUFFIX)
  for (ext in should_not_exist) {
    expect_false(file.exists(build_path_from_model(m1, ext)))
  }

  # Most arguments aren't copied...
  m1_args <- get_stanargs(m1)
  parent_args <- get_stanargs(STAN_MOD1)
  expect_false(length(m1_args) == length(parent_args))
  # ... but seed is.
  expect_false(is.null(m1_args$seed))
  expect_identical(m1_args$seed, parent_args$seed)
})

test_that("copy_model_as_stan_gq() derives default .new_model from parent", {
  tdir <- local_test_dir()
  parent <- copy_model_from(STAN_MOD3, file.path(tdir, "foo"))
  mod_gq <- copy_model_as_stan_gq(parent)
  expect_identical(get_model_id(mod_gq), "foo_gq")
})

test_that("copy_model_as_stan_gq() relays .new_model=NULL", {
  tdir <- local_test_dir()
  parent <- copy_model_from(STAN_MOD3, file.path(tdir, "100"))
  mod_gq <- copy_model_as_stan_gq(parent, .new_model = NULL)
  expect_identical(get_model_id(mod_gq), "101")
})

test_that("copy_model_as_stan_gq() sets gq_parent", {
  tdir <- local_test_dir()
  m <- copy_model_as_stan_gq(STAN_MOD1, file.path(tdir, "foo"))
  expect_identical(get_stan_gq_parent(m),
                   STAN_MOD1[[ABS_MOD_PATH]])
})

test_that("copy_model_as_stan_gq() aborts if parent is stan_gq model", {
  tdir <- local_test_dir()
  expect_error(copy_model_as_stan_gq(STAN_GQ_MOD),
               "already a stan_gq")
})

test_that("stan gq: copy_model_from() propagates gq_parent", {
  tdir <- local_test_dir()

  expect_identical(get_stan_gq_parent(STAN_GQ_MOD),
                   STAN_MOD3[[ABS_MOD_PATH]])

  m1 <- copy_model_from(STAN_GQ_MOD, file.path(tdir, "m1"))
  m2 <- copy_model_from(m1, "m2")
  expect_identical(get_stan_gq_parent(m1),
                   STAN_MOD3[[ABS_MOD_PATH]])
  expect_identical(get_stan_gq_parent(m2),
                   STAN_MOD3[[ABS_MOD_PATH]])

  m2 <- replace_all_stan_gq_parent(m2, NULL)
  m3 <- copy_model_from(m2, "m3")
  expect_null(get_stan_gq_parent(m3))
})
