% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xde_solving.R
\name{xde_steady.ode}
\alias{xde_steady.ode}
\title{Solve for the steady state of a system of equations using \link[rootSolve:steady]{rootSolve::steady}}
\usage{
\method{xde_steady}{ode}(pars)
}
\arguments{
\item{pars}{an \strong{\code{xds}} object}
}
\value{
an \strong{\code{xds}} object
}
\description{
Solve for the steady state of a system of equations using \link[rootSolve:steady]{rootSolve::steady}
}
\note{
This method dispatches on \code{class(dlay)}
}
