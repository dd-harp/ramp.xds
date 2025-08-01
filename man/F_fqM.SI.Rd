% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adult-SI.R
\name{F_fqM.SI}
\alias{F_fqM.SI}
\title{Compute Net Blood Feeding by Mosquitoes for \code{SI}}
\usage{
\method{F_fqM}{SI}(t, y, pars, s)
}
\arguments{
\item{t}{current simulation time}

\item{y}{state vector}

\item{pars}{a \link{list}}

\item{s}{the species index}
}
\value{
a \link{numeric} vector of length \code{nPatches}
}
\description{
The variable \eqn{M} is the density of  mosquitoes.
The model blood feeding \strong{parameters} are:
\itemize{
\item \eqn{f} is the overall blood feeding rate
\item \eqn{q} is the human fraction for blood feeding
The daily HBR for the human / host population strata is \eqn{\beta \cdot fqM}
}
}
