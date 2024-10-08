% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hydrology.R
\name{setup_hydrology}
\alias{setup_hydrology}
\title{Set up dynamic forcing}
\usage{
setup_hydrology(Hname, pars, setup_no_forcing = list())
}
\arguments{
\item{Hname}{the name of a model to set up}

\item{pars}{an \strong{\code{xds}} object}

\item{setup_no_forcing}{a list of options to override defaults}
}
\value{
an \strong{\code{xds}} object
}
\description{
If dynamic forcing has not
already been set up, then turn on dynamic
forcing and set all the
}
