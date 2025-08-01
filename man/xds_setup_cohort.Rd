% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xds_setup.R
\name{xds_setup_cohort}
\alias{xds_setup_cohort}
\title{Build a Model of Human / Host Cohort Dynamics}
\usage{
xds_setup_cohort(
  eir = 1,
  F_season = F_flat,
  season_par = list(),
  F_trend = F_flat,
  trend_par = list(),
  F_age = F_flat,
  age_par = list(),
  xds = "ode",
  Xname = "SIS",
  Xopts = list(),
  Xday = 1,
  HPop = 1000,
  searchB = 1,
  model_name = "unnamed"
)
}
\arguments{
\item{eir}{is the entomological inoculation rate}

\item{F_season}{a function describing a seasonal pattern over time}

\item{season_par}{parameters to configure a seasonality function using \link{make_function}}

\item{F_trend}{a function describing a temporal trend over time}

\item{trend_par}{parameters to configure a trends function using \link{make_function}}

\item{F_age}{a assigning a biting weight by age}

\item{age_par}{parameters to configure an age weights function using \link{make_function}}

\item{xds}{is \code{ode} or \code{dde} or \code{dts} for ordinary OR delay differential OR difference equations}

\item{Xname}{is a character string specifying an \strong{X} Component module}

\item{Xopts}{a list to configure the \strong{X} Component module}

\item{Xday}{is the run-time time step for \strong{X} Component (in days): integer or 1/integer}

\item{HPop}{is the number of humans in each stratum}

\item{searchB}{is a vector of search weights for blood feeding}

\item{model_name}{is a name for the model (arbitrary)}
}
\value{
an \strong{\code{xds}} object
}
\description{
\loadmathjax

A modified version of \link{xds_setup} to setup up studies of cohort
dynamics.

The \strong{\code{xds}} object defines \code{frame = class(frame) = 'cohort'} but there
is no \code{cohort} case for \link{xds_solve}. Instead, cohort
dynamics are studied using \link{xds_solve_cohort}, which was designed
to compare the outcomes for cohorts of different ages when exposure is
changing.

The interface includes options to configure a function
describing \code{F_eir} as a function of time, with seasonal components
and a trend. Exposure in a cohort is a function of its age, including
a function that modifies exposure by age.
}
\seealso{
\link{xds_setup} and \link{xds_setup_human} and \link{xds_solve_cohort}
}
