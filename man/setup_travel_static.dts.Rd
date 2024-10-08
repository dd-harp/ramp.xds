% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/travel_malaria.R
\name{setup_travel_static.dts}
\alias{setup_travel_static.dts}
\title{A function to set up malaria importation}
\usage{
\method{setup_travel_static}{dts}(
  pars,
  time_traveling = 0,
  delta = 0,
  F_season = F_flat,
  F_trend = F_flat,
  i = 1
)
}
\arguments{
\item{pars}{a \link{list}}

\item{time_traveling}{the time spent traveling}

\item{delta}{the travel FoI}

\item{F_season}{a function describing a seasonal pattern}

\item{F_trend}{a function describing a trend}

\item{i}{the host species index}
}
\value{
a \link{list}
}
\description{
Setup a static model for travel malaria
}
