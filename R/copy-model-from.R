
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
  copy_stan_model_impl(
    STAN_MODEL_REQ_FILES,
    .parent_mod = .parent_mod,
    .new_model = .new_model,
    .description = .description,
    .based_on_additional = .based_on_additional,
    .add_tags = .add_tags,
    .star = .star,
    .inherit_tags = .inherit_tags,
    .update_model_file = .update_model_file,
    .overwrite = .overwrite)
}

#' @export
copy_model_from.bbi_stan_gq_model <- function(.parent_mod,
                                              .new_model = NULL,
                                              .description = NULL,
                                              .based_on_additional = NULL,
                                              .add_tags = NULL,
                                              .star = NULL,
                                              .inherit_tags = FALSE,
                                              .update_model_file = TRUE,
                                              .overwrite = FALSE) {
  copy_stan_model_impl(
    STAN_GQ_MODEL_REQ_FILES,
    .parent_mod = .parent_mod,
    .new_model = .new_model,
    .description = .description,
    .based_on_additional = .based_on_additional,
    .add_tags = .add_tags,
    .star = .star,
    .inherit_tags = .inherit_tags,
    .update_model_file = .update_model_file,
    .overwrite = .overwrite)
}

#' Internal logic for copying a Stan model
#'
#' The main argument of interest is `files_to_copy`, which defines that set of
#' files that `copy_stan_files()` transfers to the new model. The remaining
#' arguments are relayed to bbr's `copy_model_from_impl()`.
#' @noRd
copy_stan_model_impl <- function(files_to_copy,
                                 .parent_mod,
                                 .new_model = NULL,
                                 .description = NULL,
                                 .based_on_additional = NULL,
                                 .add_tags = NULL,
                                 .star = NULL,
                                 .inherit_tags = FALSE,
                                 .update_model_file = TRUE,
                                 .overwrite = FALSE) {
  check_stan_model(.parent_mod, .error = TRUE)

  .new_model <- build_new_model_path(.parent_mod, .new_model)

  setup <- function() {
    copy_stan_files(.parent_mod,
                    .new_model,
                    .overwrite,
                    files_to_copy)
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

#' Copy a "regular" Stan model to a generated quantities model
#'
#' Like [bbr::copy_model_from()], but switch the model type from "stan" to
#' "stan_gq" begin defining a model for standalone generated quantities.
#'
#' Aside from adjusting the `model_type` value in the new model's YAML file,
#' this function does the following:
#'
#'  * adds a `<run>-fitted-param.R` file. Its value _may_ be suitable for your
#'    purposes, but please inspect it and edit as needed.
#'
#'  * copies `<run>.stan` and `<run>-standata.R` as is from the parent model. At
#'    least `<run>.stan` will need to be manually modified before running the
#'    model.
#'
#'  * `<run>-init.R` is _not_ copied because it is not relevant for standalone
#'    generated quantities.
#'
#'  * Only the `seed` argument from the parent model's `<run>-stanargs.R` is
#'    copied to the new model's `<run>-stanargs.R`.
#'
#' @inheritParams bbr::copy_model_from
#' @param .parent_mod A `bbi_stan_model` object to copy. This should _not_ be a
#'   `bbi_stan_gq_model` subclass; in that case, use [bbr::copy_model_from()] to
#'   copy the model in the standard way.
#' @export
copy_stan_model_as_gq <- function(.parent_mod,
                                  .new_model = NULL,
                                  .description = NULL,
                                  .based_on_additional = NULL,
                                  .add_tags = NULL,
                                  .star = NULL,
                                  .inherit_tags = FALSE,
                                  .overwrite = FALSE) {
  checkmate::assert_class(.parent_mod, STAN_MOD_CLASS)
  if (inherits(.parent_mod, STAN_GQ_MOD_CLASS)) {
    stop(".parent_mod (", get_model_id(.parent_mod),
         ") is already a stan_gq model.\n",
         "Use copy_model_from() instead.")
  }

  setup <- function() {
    # Exclude -stanargs.R because we only want the seed.
    #
    # TODO: Should this instead copy any arguments that are valid for
    # $generate_quantities()?
    req_files <- setdiff(STAN_GQ_MODEL_REQ_FILES, "-stanargs.R")
    copy_stan_files(.parent_mod,
                    .new_model,
                    .overwrite,
                    req_files)
    parent_args <- get_stanargs(.parent_mod)
    if (!is.null(parent_args$seed)) {
      write_stanargs(
        list(seed = parent_args$seed),
        build_path_from_new_model_path(.new_model, STANARGS_SUFFIX))
    }
  }

  # Adjust the parent type before passing it to copy_model_from_impl() so that
  # the new model ends up as a bbi_stan_gq_model object.
  class(.parent_mod) <- c(STAN_GQ_MOD_CLASS, class(.parent_mod))
  .new_model <- build_new_model_path(.parent_mod, .new_model)
  mod <- copy_model_from_impl(
    .parent_mod = .parent_mod,
    .new_model = .new_model,
    .description = .description,
    .based_on_additional = .based_on_additional,
    .add_tags = .add_tags,
    .star = .star,
    .inherit_tags = .inherit_tags,
    .overwrite = .overwrite,
    setup_fn = setup
  )

  return(mod)
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
#' @param files_to_copy Character vector of files to copy from `.parent_mod` to
#'   `.new_model`.
#' @noRd
copy_stan_files <- function(.parent_mod, .new_model, .overwrite, files_to_copy) {

  if (fs::dir_exists(.new_model)) {
    if (isTRUE(.overwrite)) {
      fs::dir_delete(.new_model)
    } else {
      stop(glue("Directory already exists at {.new_model} -- cannot copy new model files. Either pass `.overwrite = TRUE` or use `new_model({.new_model})`"))
    }
  }
  fs::dir_create(.new_model)

  purrr::walk(files_to_copy, function(.s) {
    parent_file <- build_path_from_model(.parent_mod, .s)
    if (fs::file_exists(parent_file)) {
      fs::file_copy(
        parent_file,
        build_path_from_new_model_path(.new_model, .s)
      )
    }
  })
}
