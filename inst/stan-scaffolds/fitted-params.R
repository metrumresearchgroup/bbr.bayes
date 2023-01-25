# Return a previous model fit for generating quantities of interest
#
# This function is passed one argument, the current model object (i.e. the model
# defining the standalone generated quantities). Most often, this model will be
# used to obtain the parent model with `get_stan_gq_parent(.mod)`.
#
# The return value can be any value that the `generate_quantities()` method of
# `CmdStanModel` accepts for its `fitted_params` argument. By default, paths to
# the posteriors of the `gq_parent` model are returned.
make_fitted_params <- function(.mod) {
  # Note: get_stan_gq_parent() resolves the gq_parent values (usually just one)
  # to an absolute path.
  parent <- bbr.bayes::get_stan_gq_parent(.mod)
  if (!length(parent)) {
    stop("Model ", bbr::get_model_id(.mod), " does not have gq_parent field")
  }
  if (length(parent) != 1) {
    stop("Model ", bbr::get_model_id(.mod), " has multiple gq_parent values.\n",
         "make_fitted_params() requires custom code.")
  }

  fit <- bbr.bayes::read_fit_model(bbr::read_model(parent))
  files <- fit$output_files()
  exist <- file.exists(files)
  if (!all(exist)) {
    stop("Some output files for ", bbr::get_model_id(parent),
         " are missing:\n",
         paste(" - ", files[!exist], collapse = "\n"))
  }

  return(files)
}
