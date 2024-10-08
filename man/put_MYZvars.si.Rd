% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adult-SI.R
\name{put_MYZvars.SI}
\alias{put_MYZvars.SI}
\title{Return the variables as a list}
\usage{
\method{put_MYZvars}{SI}(MYZvars, y, pars, s)
}
\arguments{
\item{MYZvars}{the variables}

\item{y}{the variables}

\item{pars}{a \link{list}}

\item{s}{the vector species index}
}
\value{
a \link{list}
}
\description{
This method dispatches on the type of \code{pars$MYZpar[[s]]}
}
