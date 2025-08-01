% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/egg_laying.R
\name{compute_Qtot}
\alias{compute_Qtot}
\title{Compute the total availability of egg-laying habitats, \eqn{Q}}
\usage{
compute_Qtot(Q, Q_ovitraps, Q_bad_habitats)
}
\arguments{
\item{Q}{the availability of ovitraps}

\item{Q_ovitraps}{the availability of ovitraps}

\item{Q_bad_habitats}{the availability of unsuitable habitats}
}
\value{
a \link{vector} of describing habitat availability, \eqn{Q}, of length \code{nPatches}
}
\description{
The sum of aquatic habitats and any other place
a mosquito might lay eggs, including ovitraps and unsuitable habitats.
}
\details{
The availability of the habitats that we have defined in the model, denoted \eqn{Q},
sums search weights, \eqn{\omega_h}, by patch
using the habitat membership matrix, \eqn{N}, and we can compute
\deqn{Q = {N} \cdot \omega_h.}
If some ovitraps and bad_habitats are also available, with values \eqn{Q_o} and \eqn{Q_b} respectively, then
\deqn{Q = Q_h + Q_o + Q_b.}
The availability of habitats, ovitraps and bad_habitats are computed elsewhere and stored on \code{pars$vars}.
}
\seealso{
This function is called by \link{make_Q}

\link{create_habitat_matrix} discusses \eqn{N}

The availability of ovitraps and bad habitats is setup in \link{setup_EGG_LAYING}
}
