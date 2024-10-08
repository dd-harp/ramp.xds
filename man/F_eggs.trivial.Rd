% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adult-trivial.R
\name{F_eggs.trivial}
\alias{F_eggs.trivial}
\title{Number of eggs laid by adult mosquitoes}
\usage{
\method{F_eggs}{trivial}(t, y, pars, s)
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
Implements \link{F_eggs} for the trivial model.
}
