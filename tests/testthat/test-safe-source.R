test_that("safe_source_function() doesn't see user workspace", {
  gvar <- "x"
  genv <- globalenv()
  if (is.null(genv[[gvar]])) {
    on.exit(rm(list = gvar, envir = genv))
    genv[[gvar]] <- 1
  }

  tfile <- withr::local_tempfile(lines = paste("foo <- function()", gvar))
  expect_error(
    safe_source_function(tfile, "foo")(),
    gvar
  )
})
