% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adult-demography.R
\name{compute_Omega_xde}
\alias{compute_Omega_xde}
\title{Make the mosquito demography matrix for spatial RM model in continuous time}
\usage{
compute_Omega_xde(g, sigma, mu, calK)
}
\arguments{
\item{g}{mosquito death rate, a vector of length \code{nPatches}}

\item{sigma}{mosquito emigration rate, a vector of length \code{nPatches}}

\item{mu}{emigration loss, a vector of length \code{nPatches}}

\item{calK}{a \link{matrix} of dimensions \code{nPatches} by \code{nPatches}}
}
\value{
a \link{matrix} of dimensions \code{nPatches} by \code{nPatches}
}
\description{
Make the mosquito demography matrix for spatial RM model in continuous time
}
