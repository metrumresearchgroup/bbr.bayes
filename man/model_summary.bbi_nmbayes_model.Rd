% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-summary.R
\name{model_summary.bbi_nmbayes_model}
\alias{model_summary.bbi_nmbayes_model}
\title{Summarize a \code{bbi_nmbayes_model} object}
\usage{
\method{model_summary}{bbi_nmbayes_model}(.mod, .bbi_args = NULL, ..., .dry_run = FALSE)
}
\arguments{
\item{.mod}{Model to summarize.}

\item{.bbi_args}{A named list specifying arguments to pass to bbi formatted like \code{list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)}.
See \code{\link[bbr:print_bbi_args]{print_bbi_args()}} for full list of options.}

\item{...}{args passed through to \code{\link[bbr:bbi_exec]{bbi_exec()}}}

\item{.dry_run}{show what the command would be without actually running it}
}
\description{
There is currently no \code{\link[bbr:model_summary]{bbr::model_summary()}} method for summarizing NONMEM
Bayes outputs. Call \code{\link[=read_fit_model]{read_fit_model()}} to return a \pkg{posterior} draws
object, which has various methods for summarizing the posteriors.
}
