context("checking Stan model integrity")

test_that("check_stan_model messages missing files", {
  tdir <- local_test_dir()
  for (model_type in c("stan", "stan_gq")) {
    .m <- new_model(file.path(tdir, model_type),
                    .model_type = model_type)

    fs::file_delete(get_model_path(.m))
    fs::file_delete(build_path_from_model(.m, STANDATA_R_SUFFIX))
    fs::file_delete(build_path_from_model(.m, STANARGS_SUFFIX))
    if (model_type == "stan") {
      fs::file_delete(build_path_from_model(.m, STANINIT_SUFFIX))
      req_files <- STAN_MODEL_REQ_FILES
    } else {
      fs::file_delete(build_path_from_model(.m, STAN_FITTED_PARAMS_SUFFIX))
      req_files <- STAN_GQ_MODEL_REQ_FILES
    }

    err_msg <- sprintf(MISSING_STAN_FILES_ERR_MSG,
                       paste0("bbi_", model_type, "_model"))
    for (.message in c(err_msg, req_files)) {
      expect_message(
        check_stan_model(.m),
        regexp = .message,
        fixed =TRUE
      )
    }
  }
})

test_that("check_stan_model messages scaffold files", {
  tdir <- local_test_dir()
  for (model_type in c("stan", "stan_gq")) {
    .m <- new_model(file.path(tdir, model_type),
                    .model_type = model_type)

    # Check all except -init.R and -fitted-params.R because they are not
    # scaffolds.
    if (model_type == "stan") {
      exclude <- STANINIT_SUFFIX
      req_files <- STAN_MODEL_REQ_FILES
    } else {
      exclude <- STAN_FITTED_PARAMS_SUFFIX
      req_files <- STAN_GQ_MODEL_REQ_FILES
    }
    files_to_check <- grep(exclude, req_files,
                           value = TRUE, invert = TRUE)

    err_msg <- sprintf(STAN_SCAFFOLD_ERR_MSG,
                       paste0("bbi_", model_type, "_model"))
    for (.message in c(err_msg, files_to_check)) {
      expect_message(
        check_stan_model(.m),
        regexp = .message
      )
    }
  }
})
