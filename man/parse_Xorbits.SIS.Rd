% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/human-SIS.R
\name{parse_Xorbits.SIS}
\alias{parse_Xorbits.SIS}
\title{Parse the output of deSolve and return variables for the SIS model}
\usage{
\method{parse_Xorbits}{SIS}(outputs, pars, i)
}
\arguments{
\item{outputs}{a \link{matrix} of outputs from deSolve}

\item{pars}{an \strong{\code{xds}} object}

\item{i}{the host species index}
}
\value{
none
}
\description{
Implements \link{parse_Xorbits} for the SIS model
}
