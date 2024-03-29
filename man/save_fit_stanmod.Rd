% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/submit-model-stan-helpers.R
\name{save_fit_stanmod}
\alias{save_fit_stanmod}
\title{Private helper to save cmdstanr model fit object to RDS}
\usage{
save_fit_stanmod(.stanmod, .out_path)
}
\arguments{
\item{.stanmod}{a \code{cmdstanr} fit object of class \code{"CmdStanMCMC"}}

\item{.out_path}{path to save the model to. Within \code{submit_model()} we pass
\code{build_path_from_model(.mod, STAN_MODEL_FIT_RDS)} but could be anywhere.}
}
\description{
This intentionally does \emph{not} collect the posteriors with \verb{$draws()} because
those are also saved to disk separately. This \emph{does} allow a user to reload
this object into memory though. (We will likely want a helper function to do
that. Maybe that's what \code{model_summary()} should do?...)
}
\keyword{internal}
