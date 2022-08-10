# This is modified from futures.callr, which is licensed under LGPL (>= 2.1).

import_from <- function(name, default = NULL, package) {
  ns <- getNamespace(package)
  if (exists(name, mode = "function", envir = ns, inherits = FALSE)) {
    get(name, mode = "function", envir = ns, inherits = FALSE)
  } else if (!is.null(default)) {
    default
  } else {
    stop(sprintf("No such '%s' function: %s()", package, name))
  }
}

import_bbr <- function(name, default = NULL) {
  import_from(name, default = default, package = "bbr")
}
