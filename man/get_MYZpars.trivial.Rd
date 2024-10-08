% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adult-trivial.R
\name{get_MYZpars.trivial}
\alias{get_MYZpars.trivial}
\title{Return the parameters as a list}
\usage{
\method{get_MYZpars}{trivial}(pars, s = 1)
}
\arguments{
\item{pars}{an \strong{\code{xds}} object}

\item{s}{the vector species index}
}
\value{
a \link{list}
}
\description{
This method dispatches on the type of \code{pars$MYZpar[[s]]}.
}
