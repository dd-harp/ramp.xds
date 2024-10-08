% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adult-macdonald.R
\name{make_MYZinits_macdonald}
\alias{make_MYZinits_macdonald}
\title{Make inits for macdonald adult mosquito model}
\usage{
make_MYZinits_macdonald(nPatches, MYZopts = list(), M = 5, P = 1, Y = 1, Z = 1)
}
\arguments{
\item{nPatches}{the number of patches in the model}

\item{MYZopts}{a \link{list} of values that overwrites the defaults}

\item{M}{total mosquito density at each patch}

\item{P}{total parous mosquito density at each patch}

\item{Y}{infected mosquito density at each patch}

\item{Z}{infectious mosquito density at each patch}
}
\value{
a \link{list}
}
\description{
Make inits for macdonald adult mosquito model
}
