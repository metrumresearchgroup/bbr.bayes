
#' Get gq_parent of a standalone generated quantities model
#'
#' For a model of type "stan_gq", the "gq_parent" field links to a model of type
#' "stan" whose samples are input as the fitted parameters.
#'
#' @param .mod A `bbi_stan_gq_model` object.
#' @return A vector of absolute paths to the models listed in the "gq_parent"
#'   field.
#' @seealso [modify_stan_gq_parent] for modifying "gq_parent" value, [bbr_stan]
#'   for more information about standalone generated quantities
#' @export
get_stan_gq_parent <- function(.mod) {
  checkmate::assert_class(.mod, STAN_GQ_MOD_CLASS)
  resolved <- get_stan_gq_parent_no_check(.mod)
  if (!is.null(resolved)) {
    check_gq_parent(resolved)
  }

  return(resolved)
}

get_stan_gq_parent_no_check <- function(.mod) {
  gq_parent <- .mod[[YAML_GQ_PARENT]]
  if (is.null(gq_parent) || !length(gq_parent)) {
    # ^ Check length() to get a consistent NULL return value even when
    # modify_model_field() leaves .mod's YAML_GQ_PARENT at character(0) (which
    # won't be written out to the YAML).
    return(NULL)
  }
  resolve_gq_parent(get_model_working_directory(.mod),
                    gq_parent)
}

#' Modify gq_parent of a standalone generated quantities model
#'
#' @description
#'
#' In most cases, the "gq_parent" field is automatically handled by
#' [copy_model_as_stan_gq()]. However, the following convenience functions
#' enable manually modifying the value if needed:
#'
#'   * `add_stan_gq_parent()`: add specified values, keeping any existing values
#'
#'   * `replace_all_stan_gq_parent()`: replace all existing values with the
#'      specified values
#'
#'   * `remove_stan_gq_parent()`: remove the specified values
#'
#' @param .mod A `bbi_stan_gq_model` object.
#' @param .parent Character vector of paths to parent model. If relative, the
#'   paths are taken as relative to the directory containing `.mod`.
#' @return The modified model objects (after writing changes to disk).
#' @seealso [modify_stan_gq_parent] for getting the "gq_parent" value,
#'   [bbr_stan] for more information about standalone generated quantities
#' @name modify_stan_gq_parent
NULL

#' @rdname modify_stan_gq_parent
#' @export
add_stan_gq_parent <- function(.mod, .parent) {
  checkmate::assert_character(.parent)
  modify_gq_parent(.mod, .parent, .append = TRUE)
}

#' @rdname modify_stan_gq_parent
#' @export
replace_all_stan_gq_parent <- function(.mod, .parent) {
  checkmate::assert_character(.parent, null.ok = TRUE)
  modify_gq_parent(.mod, .parent, .append = FALSE)
}

#' @rdname modify_stan_gq_parent
#' @export
remove_stan_gq_parent <- function(.mod, .parent) {
  checkmate::assert_character(.parent, null.ok = TRUE)
  modify_gq_parent(.mod, .parent, .append = FALSE, .remove = TRUE)
}

modify_gq_parent <- function(.mod, .parent, ...) {
  checkmate::assert_class(.mod, STAN_GQ_MOD_CLASS)

  if (is.null(.parent)) {
    values <- .parent
  } else {
    start <- get_model_working_directory(.mod)
    .parent <- fs::path_ext_remove(.parent)
    values <- fs::path_rel(fs::path_abs(.parent, start = start),
                           start = start)
    check_gq_parent(resolve_gq_parent(start, values))
  }

  bbr::modify_model_field(
    .mod = .mod,
    .field = YAML_GQ_PARENT,
    .value = values,
    ...)
}

resolve_gq_parent <- function(start, paths) {
  paths_abs <- file.path(start, fs::path_ext_remove(paths))
  # Normalize path for consistency with bbr (in particular get_based_on() and
  # new_model()).
  return(as.character(fs::path_norm(paths_abs)))
}

#' Abort if any gq_parent values have missing .yaml files
#'
#' @param paths Character vector of absolute paths.
#' @noRd
check_gq_parent <- function(paths) {
  # Note that this error check is meant to cover the same the error checking
  # that bbr's safe_based_on() does for based_on values.
  yaml_exist <- fs::file_exists(fs::path_ext_set(paths, "yaml"))
  if (!all(yaml_exist)) {
    stop("Some `parent` models are missing .yaml files:\n",
         paste("  -", paths[!yaml_exist], collapse = "\n"),
         call. = FALSE)
  }
}

#' Search for models that have a given model as the gq_parent
#'
#' By looking at the "gq_parent" field of a "stan_gq" model, you can identify
#' which "stan" models are used as input. This function helps you go in the
#' other direction: find which "stan_gq" models under a directory point to the
#' specified "stan" model.
#'
#' @inheritParams bbr::run_log
#' @param .mod A `bbi_stan_model` object.
#' @param .base_dir Base directory to look in for models. If `NULL` (the
#'   default), look for models in the directory that contains `.mod`.
#' @seealso [get_stan_gq_parent()] and [modify_stan_gq_parent] for getting and
#'   modifying the "gq_parent" value of a `bbi_stan_gq_model` object, [bbr_stan]
#'   for more information about standalone generated quantities
#' @export
find_stan_gq_children <- function(.mod,
                                  .base_dir = NULL,
                                  .recurse = FALSE) {
  checkmate::assert_class(.mod, STAN_MOD_CLASS)
  if (inherits(.mod, STAN_GQ_MOD_CLASS)) {
    stop(".mod must be model_type=stan, not model_type=stan_gq")
  }

  .base_dir <- .base_dir %||% get_model_working_directory(.mod)
  mod_path <- .mod[[ABS_MOD_PATH]]

  is_child <- function(cand) {
    inherits(cand, STAN_GQ_MOD_CLASS) &&
      mod_path %in% get_stan_gq_parent_no_check(cand)
  }

  find_models(.base_dir = .base_dir, .recurse = .recurse, .include = NULL) %>%
    purrr::keep(is_child)
}
