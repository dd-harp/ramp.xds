% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aquatic-basicL.R
\name{Update_Lt.basicL}
\alias{Update_Lt.basicL}
\title{Update State Variables for \code{basicL} (\strong{L} Component)}
\usage{
\method{Update_Lt}{basicL}(t, y, pars, s)
}
\arguments{
\item{t}{current simulation time}

\item{y}{state vector}

\item{pars}{an \strong{\code{xds}} object}

\item{s}{the species index}
}
\value{
a \link{numeric} vector
}
\description{
Implements \link{Update_Lt} for the \code{basicL} competition model.
}
