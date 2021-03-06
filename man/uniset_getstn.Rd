% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_functions.R
\name{uniset_getstn}
\alias{uniset_getstn}
\title{Get Settings Object
description Source the list holding the key=value pairs from the settings.R
file.}
\usage{
uniset_getstn(uniset_handover)
}
\arguments{
\item{uniset_handover}{List length two, containing two elements:
\enumerate{
\item \code{pkgname}: The name of the target package.
\item \code{funcname}: The name of the function in the target package handing over
the required values. See examples at \code{\link{uniset}}.
}}
}
\value{
A list holding the key=value pairs from the settings.R file on
on success, NULL if sourcing the file was not successful.
}
\description{
Get Settings Object
description Source the list holding the key=value pairs from the settings.R
file.
}
\section{Important}{
 This function is meant to be called from within the
target package.
}

\examples{
{
\dontrun{
# to be called from within the target package
uniset_getstn(uniset_handover)
}
}
}
