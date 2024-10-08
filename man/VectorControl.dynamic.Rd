% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector_control.R
\name{VectorControl.dynamic}
\alias{VectorControl.dynamic}
\title{Implement Some Vector Control}
\usage{
\method{VectorControl}{dynamic}(t, y, pars)
}
\arguments{
\item{t}{current simulation time}

\item{y}{state vector}

\item{pars}{an \strong{\code{xds}} object}
}
\value{
a named \link{list}
}
\description{
Implements various forms
of vector control. Each mode for vector
control is set up and configured separately.
}
\note{
This a junction to implement various modes of
vector control.
Non-trivial vector control modules are in
\href{https://github.com/dd-harp/ramp.control}{\strong{\code{ramp.control}}}.
}
