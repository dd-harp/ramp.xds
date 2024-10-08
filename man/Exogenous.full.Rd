% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exogenous.R
\name{Exogenous.full}
\alias{Exogenous.full}
\title{Generalized spatial differential equation model}
\usage{
\method{Exogenous}{full}(t, y, pars)
}
\arguments{
\item{t}{current simulation time}

\item{y}{state vector}

\item{pars}{an \strong{\code{xds}} object}
}
\value{
a \link{list} containing the vector of all state derivatives
}
\description{
Compute exogenous forcing variables
}
