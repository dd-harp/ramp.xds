% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/human-SIS.R
\name{update_Xinits.SIS}
\alias{update_Xinits.SIS}
\title{Update inits for the SIS xde human model from a vector of states}
\usage{
\method{update_Xinits}{SIS}(pars, y0, i = 1)
}
\arguments{
\item{pars}{an \strong{\code{xds}} object}

\item{y0}{a vector of initial values}

\item{i}{the host species index}
}
\value{
an \code{xds} object
}
\description{
Update inits for the SIS xde human model from a vector of states
}
