% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adult-SI.R
\name{parse_MYZorbits.SI}
\alias{parse_MYZorbits.SI}
\title{Parse the output of deSolve and return variables for the \code{SI} model}
\usage{
\method{parse_MYZorbits}{SI}(outputs, pars, s)
}
\arguments{
\item{outputs}{a \link{matrix} of outputs from deSolve}

\item{pars}{a \link{list} that defines a model}

\item{s}{the species index}
}
\value{
a \link{list}
}
\description{
Implements \link{parse_MYZorbits} for the \code{SI} model
}
