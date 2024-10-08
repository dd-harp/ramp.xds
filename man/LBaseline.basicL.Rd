% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aquatic-basicL.R
\name{LBaseline.basicL}
\alias{LBaseline.basicL}
\title{Baseline Bionomics for \code{basicL} (\strong{L} Component)}
\usage{
\method{LBaseline}{basicL}(t, y, pars, s)
}
\arguments{
\item{t}{current simulation time}

\item{y}{state vector}

\item{pars}{an \code{xds} object}

\item{s}{the species index}
}
\value{
a \strong{\code{ramp.xds}} object
}
\description{
Set \strong{L} component parameters
to baseline values for \code{basicL}
}
