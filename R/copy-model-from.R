
#' @export
copy_model_from.bbi_stan_model <- function(
  .parent_mod,
  .new_model = NULL,
  .description = NULL,
  .based_on_additional = NULL,
  .add_tags = NULL,
  .star = NULL,
  .inherit_tags = FALSE,
  .update_model_file = TRUE,
  .overwrite = FALSE
) {

  check_stan_model(.parent_mod, .error = TRUE)

  .new_model <- build_new_model_path(.parent_mod, .new_model)

  setup <- function() {
    copy_stan_files(.parent_mod,
                    .new_model,
                    .overwrite)
  }

  .mod <- copy_model_from_impl(
    .parent_mod = .parent_mod,
    .new_model = .new_model,
    .description = .description,
    .based_on_additional = .based_on_additional,
    .add_tags = .add_tags,
    .star = .star,
    .inherit_tags = .inherit_tags,
    .update_model_file = .update_model_file,
    .overwrite = .overwrite,
    setup_fn = setup
  )

  return(.mod)
}

# In copy_model_from context, we can't use real build_path_from_model() because
# the new model doesn't exist yet.
build_path_from_new_model_path <- function(.new_model, .suffix) {
  file.path(.new_model, paste0(basename(.new_model), .suffix))
}

#' Copy necessary Stan files
#'
#' Helper function to copy a necessary Stan files from parent model to new model
#' and rename them appropriately. Note that any directory existing at
#' `.new_model` will be overwritten by default.
#' @param .parent_mod a `bbi_stan_model` object to copy from
#' @param .new_model Path to new model directory
#' @param .overwrite If `TRUE`, overwrite existing directory at `.new_model`. If `FALSE` and directory exists at `.new_model` error.
#' @noRd
copy_stan_files <- function(.parent_mod, .new_model, .overwrite) {

  if (fs::dir_exists(.new_model)) {
    if (isTRUE(.overwrite)) {
      fs::dir_delete(.new_model)
    } else {
      stop(glue("Directory already exists at {.new_model} -- cannot copy new model files. Either pass `.overwrite = TRUE` or use `new_model({.new_model})`"))
    }
  }
  fs::dir_create(.new_model)

  purrr::walk(STAN_MODEL_REQ_FILES, function(.s) {
    parent_file <- build_path_from_model(.parent_mod, .s)
    if (fs::file_exists(parent_file)) {
      fs::file_copy(
        parent_file,
        build_path_from_new_model_path(.new_model, .s)
      )
    }
  })
}
