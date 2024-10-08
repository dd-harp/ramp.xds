% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/human-SIS.R
\name{F_ni.SIS}
\alias{F_ni.SIS}
\title{Compute the net infectiousness}
\usage{
\method{F_ni}{SIS}(vars, Xpar)
}
\arguments{
\item{vars}{a list with the variables attached by name}

\item{Xpar}{a list defining a model for human}
}
\value{
a \link{numeric} vector of length \code{nStrata}
}
\description{
Implements \link{F_ni} for the SIS model.
}
