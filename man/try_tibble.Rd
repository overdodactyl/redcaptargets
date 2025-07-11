% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap.R
\name{try_tibble}
\alias{try_tibble}
\title{Convert a data object to a tibble if \code{tibble} is installed}
\usage{
try_tibble(.data)
}
\arguments{
\item{.data}{A data frame–like object.}
}
\value{
A tibble (if \code{tibble} is installed) or the original object.
}
\description{
A lightweight helper that attempts to coerce \code{.data} to a tibble
using \code{\link[tibble:as_tibble]{tibble::as_tibble()}}—but only if the \code{tibble} package is installed.
Otherwise, it silently returns the input unmodified.
}
\details{
This is useful for optional tibble formatting without forcing \code{tibble}
to be a hard dependency of the package.
}
\keyword{internal}
