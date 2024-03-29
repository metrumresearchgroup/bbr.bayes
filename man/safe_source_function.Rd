% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safe-source.R
\name{safe_source_function}
\alias{safe_source_function}
\title{Private helper to safely source a specific function from a file}
\usage{
safe_source_function(.file, .func_name)
}
\arguments{
\item{.file}{(String) file path to file to source.}

\item{.func_name}{(String) name of function to return. A function with this
name must exist in \code{.file} or an error will be thrown.}
}
\description{
Private helper to safely source a specific function from a file
}
\keyword{internal}
