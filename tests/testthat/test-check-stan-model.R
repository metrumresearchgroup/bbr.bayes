context("checking Stan model integrity")

test_that("check_stan_object messages missing files", {
  tdir <- local_test_dir()
  .m <- new_model(file.path(tdir, "stanmod"), .model_type = "stan")

  fs::file_delete(get_model_path(.m))
  fs::file_delete(build_path_from_model(.m, STANDATA_R_SUFFIX))
  fs::file_delete(build_path_from_model(.m, STANARGS_SUFFIX))
  fs::file_delete(build_path_from_model(.m, STANINIT_SUFFIX))

  for (.message in c(MISSING_STAN_FILES_ERR_MSG, STAN_MODEL_REQ_FILES)) {
    expect_message(
      check_stan_model(.m),
      regexp = .message
    )
  }
})

test_that("check_stan_object messages scaffold files", {
  tdir <- local_test_dir()
  .m <- new_model(file.path(tdir, "stanmod"), .model_type = "stan")

  # check all except init (because it's not a scaffold)
  files_to_check <- grep(STANINIT_SUFFIX, STAN_MODEL_REQ_FILES, value = TRUE, invert = TRUE)
  for (.message in c(STAN_SCAFFOLD_ERR_MSG, files_to_check)) {
    expect_message(
      check_stan_model(.m),
      regexp = .message
    )
  }
})
