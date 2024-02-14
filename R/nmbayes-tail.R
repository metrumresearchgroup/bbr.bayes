#' Inspect first and last lines of select files from a NONMEM Bayes model
#'
#' `tail_lst()` and `tail_output()` provide [bbr::tail_lst()] and
#' [bbr::tail_output()] methods for `bbi_nmbayes_model` objects. Compared to the
#' methods for `bbi_nonmem_model` objects, the key difference is that these
#' operate on the `*.lst` or `OUTPUT` file of each chain submodel.
#'
#' @inheritParams bbr::check_file
#' @param .return If `TRUE`, return the head and tail results for each chain.
#'   See \sQuote{Value} section for more details.
#' @param ... Arguments passed to method call on each chain submodel.
#' @return If `.return` is `TRUE`, return a list of character vectors, with an
#'   element for each chain. The character vector is comprised of the first and
#'   last lines of the file under a chain subdirectory. For `tail_output()`, the
#'   element will be `NULL` if 1) the model has started to run but hasn't yet
#'   created the `OUTPUT` file or 2) the model has finished running.
#' @seealso [chain_paths()] for constructing chain submodels paths
#' @name nmbayes_tail
#' @aliases nmbayes_tail_lst nmbayes_tail_output
NULL

#' @rdname nmbayes_tail
#' @export
tail_lst.bbi_nmbayes_model <- function(.mod, .head = 3, .tail = 5,
                                       .print = TRUE, .return = FALSE, ...) {
  tail_submodels(tail_lst, .mod, .head, .tail, .print, .return)
}

#' @rdname nmbayes_tail
#' @export
tail_output.bbi_nmbayes_model <- function(.mod, .head = 3, .tail = 5,
                                          .print = TRUE, .return = FALSE, ...) {
  tail_submodels(tail_output, .mod, .head, .tail, .print, .return)
}

tail_submodels <- function(fn, .mod, .head, .tail, .print, .return, ...) {
  chain_dirs <- get_chain_dirs(.mod)
  res <- vector("list", length = length(chain_dirs))
  errors <- character()
  for (i in seq_along(chain_dirs)) {
    if (isTRUE(.print)) {
      cli::cli_h1("Chain {i}")
    }
    submod <- read_model(chain_dirs[i])
    out <- tryCatch(
      fn(
        submod,
        .head = .head, .tail = .tail, .print = .print, .return = .return,
        ...
      ),
      error = identity
    )
    if (inherits(out, "error")) {
      errors <- c(errors, sprintf("chain %d: %s", i, conditionMessage(out)))
    } else if (!is.null(out)) {
      res[[i]] <- out
    }
  }

  if (length(errors)) {
    rlang::abort(c("Some chains had errors.", errors))
  }

  if (isTRUE(.return)) {
    return(res)
  }
  return(invisible(NULL))
}
