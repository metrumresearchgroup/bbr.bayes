
#' Get gq_parent of a standalone generated quantities model
#'
#' @param .mod A `bbi_stan_gq_model` object.
#' @return A vector of absolute paths to the models listed in the "gq_parent"
#'   field.
#' @export
get_stan_gq_parent <- function(.mod) {
  checkmate::assert_class(.mod, STAN_GQ_MOD_CLASS)
  gq_parent <- .mod[[YAML_GQ_PARENT]]
  if (is.null(gq_parent) || !length(gq_parent)) {
    # ^ Check length() to get a consistent NULL return value even when
    # modify_model_field() leaves .mod's YAML_GQ_PARENT at character(0) (which
    # won't be written out to the YAML).
    return(NULL)
  }
  resolved <- resolve_gq_parent(get_model_working_directory(.mod),
                                gq_parent)
  check_gq_parent(resolved)

  return(resolved)
}

#' Modify gq_parent of a standalone generated quantities model
#'
#' @param .mod A `bbi_stan_gq_model` object.
#' @param .parent Character vector of paths to parent model. If relative, the
#'   paths are taken as relative to the directory containing `.mod`.
#' @return The modified model objects (after writing changes to disk).
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
