% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/human-demography-births.R
\name{setup_births_static}
\alias{setup_births_static}
\title{Setup a static birth_rate}
\usage{
setup_births_static(pars, i, birth_rate)
}
\arguments{
\item{pars}{a \link{list}}

\item{i}{the host species index}

\item{birth_rate}{a birth rate vector}
}
\value{
an \strong{\code{xds}} object
}
\description{
Each model determines the compartment for
births. The birth rate should be a vector of length \code{nStrata}
where the entries are zero for all but the stratum that
gets newborns.
}
