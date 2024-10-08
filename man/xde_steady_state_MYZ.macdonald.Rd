% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adult-macdonald.R
\name{xde_steady_state_MYZ.macdonald}
\alias{xde_steady_state_MYZ.macdonald}
\title{Compute the steady states as a function of the daily EIR}
\usage{
\method{xde_steady_state_MYZ}{macdonald}(Lambda, kappa, MYZpar)
}
\arguments{
\item{Lambda}{the daily emergence rate of adult mosquitoes}

\item{kappa}{net infectiousness}

\item{MYZpar}{a list that defines an adult model}
}
\value{
none
}
\description{
This method dispatches on the type of \code{MYZpar}.
}
