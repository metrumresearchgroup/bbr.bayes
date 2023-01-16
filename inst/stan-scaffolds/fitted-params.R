# Return a previous model fit for generating quantities of interest
#
# This function is passed one argument, the current model object (i.e. the model
# defining the standalone generated quantities). Most often, this model will be
# used to obtain the parent model by extracting the left-most item returned by
# `get_based_on(.mod)`.
#
# The return value can be any value that the `generate_quantities()` method of
# `CmdStanModel` accepts for its `fitted_params` argument. By default, paths to
# the _parent_ model's posteriors are returned.
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
