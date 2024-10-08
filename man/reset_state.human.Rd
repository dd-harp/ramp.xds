% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reset_state.R
\name{reset_state.human}
\alias{reset_state.human}
\title{Compute other variables at time t}
\usage{
\method{reset_state}{human}(t, y, pars)
}
\arguments{
\item{t}{current simulation time}

\item{y}{state vector}

\item{pars}{a \link{list}}
}
\value{
\strong{pars} a \link{list}
}
\description{
Compute everything but the derivatives for the generalized
spatial differential equation model
}
