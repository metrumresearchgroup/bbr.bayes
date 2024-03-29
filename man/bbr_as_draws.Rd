% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as-draws.R
\name{bbr_as_draws}
\alias{bbr_as_draws}
\alias{as_draws.bbi_nmbayes_model}
\alias{as_draws.bbi_stan_model}
\title{Extract \pkg{posterior} draws from a bbr.bayes model}
\usage{
\method{as_draws}{bbi_nmbayes_model}(x, ...)

\method{as_draws}{bbi_stan_model}(x, ...)
}
\arguments{
\item{x}{Model object.}

\item{...}{Additional arguments used by specific methods.}
}
\value{
A \pkg{posterior} draws object.
}
\description{
bbr.bayes models define methods for \code{\link[posterior:draws]{posterior::as_draws()}} that enable
constructing a draws object directly from a model object. In addition to
\code{as_draws()}, format-specific functions, such as \code{\link[posterior:draws_df]{posterior::as_draws_df()}}
and \code{\link[posterior:draws_rvars]{posterior::as_draws_rvars()}}, are also supported.
}
\details{
It is possible to get the same values via \code{\link[=read_fit_model]{read_fit_model()}}, though the
details depend on the model type.
\itemize{
\item \code{bbi_nmbayes_model}: \code{as_draws()} and friends behaves the same as calling
\code{\link[=read_fit_model]{read_fit_model()}}.
\item \code{bbi_stan_model}: \code{as_draws()} and friends are a shortcut for calling the
same methods on the \code{CmdStanMCMC} object returned by \code{\link[=read_fit_model]{read_fit_model()}}
or for using the \code{draws} method of the \code{CmdStanMCMC} object.
}
}
