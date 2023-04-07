#####################################
# PRIVATE HELPER FUNCTIONS
# for sourcing functions from a file
#####################################

#' Private helper to safely source a specific function from a file
#' @param .file (String) file path to file to source.
#' @param .func_name (String) name of function to return. A function with this
#'   name must exist in `.file` or an error will be thrown.
#' @keywords internal
safe_source_function <- function(.file, .func_name) {
  checkmate::assert_string(.file)
  checkmate::assert_string(.func_name)

  # Use global environment's parent, as opposed to base environment, to increase
  # isolation while not requiring the user to qualify names for packages on
  # their search path (e.g., they can still use "rnorm" rather than
  # "stats::rnorm").
  .env <- new.env(parent = parent.env(globalenv()))
  tryCatch(
    source(.file, local = .env),
    error = function(.e) {
      err_msg <- paste(
        glue("Loading `{.func_name}()` from {.file} FAILED with the following:"),
        .e$message
      )
      stop(err_msg, call. = FALSE)
    }
  )

  .func <- .env[[.func_name]]
  if(is.null(.func)) {
    stop(glue("{.file} must contain a function called `{.func_name}` but it does not."), call. = FALSE)
  }

  return(.func)
}

#' Private helper to safely call a function from a sourced file
#' @param .func The function to call
#' @param .args named list of args to call the function with (uses `do.call()`)
#' @param .file (optional) file path to file that was sourced (for friendlier
#'   error message)
#' @param .expected_class (optional) name of class that the result of calling
#'   `.func` should return. If _not_ `NULL` will error if the return value
#'   doesn't inherit this class.
#' @keywords internal
safe_call_sourced <- function(.func, .args, .file = NULL, .expected_class = NULL) {
  fname <- as.character(substitute(.func))
  .res <- tryCatch(
    do.call(.func, .args),
    error = function(.e) {
      err_msg <- paste(
        glue("Calling `{fname}()` from {.file} FAILED with the following:"),
        .e$message
      )
      stop(err_msg, call. = FALSE)
    }
  )

  if (!is.null(.expected_class) && !inherits(.res, .expected_class)) {
    err_msg <- paste(
      glue("The result of `{fname}()` was expected to be {.expected_class} but got the following:"),
      paste(class(.res), collapse = ", ")
    )
    stop(err_msg, call. = FALSE)
  }

  return(.res)
}
