# This is modified from futures.callr, which is licensed under LGPL (>= 2.1).

import_from <- function(name, default = NULL, mode = "function", package) {
  ns <- getNamespace(package)
  if (exists(name, mode = mode, envir = ns, inherits = FALSE)) {
    get(name, mode = mode, envir = ns, inherits = FALSE)
  } else if (!is.null(default)) {
    default
  } else {
    stop(sprintf("No such '%s' member (mode=%s): %s", package, mode, name))
  }
}

import_bbr <- function(name, default = NULL, mode = "function") {
  import_from(name, default = default, mode = mode, package = "bbr")
}
