% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stan-gq-parent.R
\name{get_stan_gq_parent}
\alias{get_stan_gq_parent}
\title{Get gq_parent of a standalone generated quantities model}
\usage{
get_stan_gq_parent(.mod)
}
\arguments{
\item{.mod}{A \code{bbi_stan_gq_model} object.}
}
\value{
A vector of absolute paths to the models listed in the "gq_parent"
field.
}
\description{
For a model of type "stan_gq", the "gq_parent" field links to a model of type
"stan" whose samples are input as the fitted parameters.
}
\seealso{
\link{modify_stan_gq_parent} for modifying "gq_parent" value, \link{bbr_stan}
for more information about standalone generated quantities
}
