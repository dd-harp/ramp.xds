% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adult-bionomics.R
\name{F_sigma}
\alias{F_sigma}
\title{Compute mosquito emigration rates}
\usage{
F_sigma(t, vars, sigma_par)
}
\arguments{
\item{t}{current simulation time}

\item{vars}{exogenous variables}

\item{sigma_par}{a \link{list}}
}
\value{
a \link{numeric} vector of length \code{nPatches}
}
\description{
This method dispatches on the type of \code{q_par$sigma_par}. It should
set the values of sigma to (possibly changing) baseline value(s).
}
