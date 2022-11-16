skip_long_tests("skipping long-running nmbayes submit_model tests")

if (identical(Sys.getenv("METWORX_VERSION"), "")) {
  skip("test-workflow-nmbayes only runs on Metworx")
}

# TODO: Consider reworking this helper to be used elsewhere.
local_model_tempdir <- function(clean = TRUE, .local_envir = parent.frame()) {
  tdir <- withr::local_tempdir("bbr-bayes-nmbayes-",
                               clean = clean, .local_envir = .local_envir)
  withr::local_dir(tdir, .local_envir = .local_envir)

  fs::dir_copy(
    system.file("extdata", package = "bbr.bayes", mustWork = TRUE),
    ".")

  mdir <- file.path("model", "nonmem", "bayes")
  fs::dir_create(mdir)
  bbr::bbi_init(mdir, "/opt/NONMEM", "nm75")

  parent_mod_path <- system.file(mdir, "1100",
                                 package = "bbr.bayes", mustWork = TRUE)
  fs::dir_copy(parent_mod_path, mdir)
  fs::file_copy(paste0(parent_mod_path, c(".yaml", ".ctl")),
                mdir)

  bbr::read_model(file.path(mdir, "1100")) %>%
    copy_model_from("1101") %>%
    bbr::update_model_id()
}

local_model_tempdir()

test_that("nmbayes: submit_model() works", {
  mod_path <- file.path("model", "nonmem", "bayes", "1101")
  mod <- read_model(mod_path)
  # Even with only two cores, this shouldn't take over two minutes.
  res <- submit_model(mod, .mode = "local")

  # This relays the return value of submit_models().
  expect_length(res, 1)
  expect_s3_class(res[[1]], "bbi_process")
})
