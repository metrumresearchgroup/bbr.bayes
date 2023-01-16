make_fitted_params <- function(.mod) {
  # Note: get_based_on() resolves the based_on values to an absolute path.
  based_on <- bbr::get_based_on(.mod)
  if (!length(based_on)) {
    stop("Model ", bbr::get_model_id(.mod), " does not have parent")
  }

  parent <- bbr::read_model(based_on[1])
  fit <- bbr.bayes::read_fit_model(parent)
  files <- fit$output_files()
  exist <- file.exists(files)
  if (!all(exist)) {
    stop("Some output files for ", bbr::get_model_id(parent),
         " are missing:\n",
         paste(" - ", files[!exist], collapse = "\n"))
  }

  return(files)
}
