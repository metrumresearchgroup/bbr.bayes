
### NONMEM Bayes

#' Copy a "regular" NONMEM model to NONMEM Bayes model
#'
#' Like [bbr::copy_model_from()], but switch the model type from "nonmem" to
#' "nmbayes" to begin defining a [NONMEM Bayes model][bbr_nmbayes].
#'
#' @inheritParams bbr::copy_model_from
#' @param .parent_mod A `bbi_nonmem_model` object to copy. This should _not_ be
#'   a `bbi_nmbayes_model` subclass; in that case, use [bbr::copy_model_from()]
#'   to copy the model in the standard way.
#' @param .update_model_file Whether to update the newly created model file. If
#'   `TRUE`, the model will be adjusted by [bbr::copy_model_from()]. In
#'   addition, any existing estimation records will be deleted, and the two
#'   estimation records required for nmbayes models will be added along with a
#'   comment.
#' @return A `bbi_nmbayes_model` object for the new model.
#' @seealso [bbr_nmbayes] for a high-level description of how NONMEM Bayes
#'   models are structured in bbr
#' @export
copy_model_as_nmbayes <- function(.parent_mod,
                                  .new_model = NULL,
                                  .description = NULL,
                                  .based_on_additional = NULL,
                                  .add_tags = NULL,
                                  .star = NULL,
                                  .inherit_tags = FALSE,
                                  .update_model_file = TRUE,
                                  .overwrite = FALSE) {
  if (!requireNamespace("nmrec", quietly = TRUE)) {
    stop("nmbayes functionality requires nmrec package.")
  }

  checkmate::assert_class(.parent_mod, NM_MOD_CLASS)
  if (inherits(.parent_mod, NMBAYES_MOD_CLASS)) {
    stop(".parent_mod (", get_model_id(.parent_mod),
         ") is already an nmbayes model.\n",
         "Use copy_model_from() instead.")
  }

  mod <- copy_model_from(
    .parent_mod = .parent_mod,
    .new_model = .new_model,
    .description = .description,
    .based_on_additional = .based_on_additional,
    .add_tags = .add_tags,
    .star = .star,
    .inherit_tags = .inherit_tags,
    .update_model_file = .update_model_file,
    .overwrite = .overwrite
  )

  mod[[YAML_MOD_TYPE]] <- "nmbayes"
  save_model_yaml(mod)

  if (isTRUE(.update_model_file)) {
    msg <- glue::glue_data_safe(list(model_id = get_model_id(mod)),
                                NMBAYES_HELP, .trim = FALSE)
    ctl_file <- get_model_path(mod)
    ctl <- nmrec::read_ctl(ctl_file)
    ests <- nmrec::select_records(ctl, "estimation")
    if (length(ests)) {
      for (est in ests) {
        est$parse()
        est$values <- trailing_comments(est$values)
      }
      ests[[1]]$values <- c(msg, ests[[1]]$values)
    } else {
      ctl$records <- c(ctl$records, msg)
    }
    nmrec::write_ctl(ctl, ctl_file)
  }

  return(read_model(mod[[ABS_MOD_PATH]]))
}

#' Return trailing comments of record, if any
#'
#' @param values nmrec_record object values
#' @noRd
trailing_comments <- function(values) {
  idx_opt <- purrr::detect_index(values,
    function(x) inherits(x, "nmrec_option"),
    .dir = "backward")

  nvals <- length(values)

  idx_lb <- idx_opt + purrr::detect_index(
    values[idx_opt:nvals],
    function(x) inherits(x, "nmrec_linebreak"))

  tail <- values[idx_lb:nvals]
  if (!purrr::some(tail, function(x) inherits(x, "nmrec_comment"))) {
    return(list())
  }
  return(tail)
}

### Stan

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
  mod <- copy_stan_model_impl(
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

  gq_parent <- get_stan_gq_parent(.parent_mod)
  if (!is.null(gq_parent)) {
    mod <- add_stan_gq_parent(mod, gq_parent)
  }

  return(mod)
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
  check_stan_model(.parent_mod, .syntax = FALSE, .error = TRUE)

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
#'  * Sets "gq_parent" field of `.new_model` to point to `.parent_mod`.
#'
#' @inheritParams bbr::copy_model_from
#' @param .parent_mod A `bbi_stan_model` object to copy. This should _not_ be a
#'   `bbi_stan_gq_model` subclass; in that case, use [bbr::copy_model_from()] to
#'   copy the model in the standard way.
#' @param .new_model Path to the new model, either absolute or relative to the
#'   path to `.parent_mod`. By default, this is the parent's model name with
#'   "_gq" appended. Aside from the default value, this is handled in the same
#'   way as the `.new_model` argument of [bbr::copy_model_from()].
#' @return A `bbi_stan_gq_model` object for the new model.
#' @seealso [bbr_stan] for a high-level description of how Stan models are
#'   structured
#' @export
copy_model_as_stan_gq <- function(.parent_mod,
                                  .new_model = paste0(bbr::get_model_id(.parent_mod),
                                                      "_gq"),
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
  mod <- add_stan_gq_parent(mod, .parent_mod[[ABS_MOD_PATH]])

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
