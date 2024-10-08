% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blood_feeding.R
\name{BloodFeeding.setup}
\alias{BloodFeeding.setup}
\title{Compute blood feeding objects: setup for static models}
\usage{
\method{BloodFeeding}{setup}(t, y, pars)
}
\arguments{
\item{t}{the time}

\item{y}{the state variables}

\item{pars}{an \code{xds} object}
}
\value{
an \code{xds} object
}
\description{
This sets up host availability, \eqn{W},
total blood host availability, \eqn{B},
the time spent matrix \eqn{\Theta}, and the time-at-risk matrix \eqn{\Psi}
for static models.
}
\details{
The mixing matrix, \eqn{\beta}, depends on
blood feeding terms, so the class of \code{pars$beta} must also
be updated, if they are not dynamic, so \link{trigger_setup} is called.
}
