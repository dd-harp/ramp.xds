% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/egg_laying.R
\name{create_habitat_matrix}
\alias{create_habitat_matrix}
\title{Create the habitat membership matrix, \eqn{N}}
\usage{
create_habitat_matrix(nPatches, membership)
}
\arguments{
\item{nPatches}{the number of patches, \eqn{n_p}}

\item{membership}{a vector describing the patch index for each habitat}
}
\value{
the habitat membership \link{matrix}, denoted \eqn{N} where \eqn{\left|N\right|= n_p \times n_q}
}
\description{
The habitat membership matrix, \eqn{N}, holds
information about the patch location of each habitat. It is part of
the egg-laying and emergence interface, making it possible
to compute egg laying from patches to habitats; and emergence from habitats to patches.
}
\details{
Information about the patch location of each habitat
is passed as the membership vector, an ordered list of patch locations. If
the \eqn{i^{th}} habitat is in the \eqn{j^{th}} patch, then
\eqn{{N}_{j,i}=1.} Otherwise, \eqn{{N}_{j,i}=0.}

Since \eqn{N} is a matrix, it is readily used for computation. Let:
\itemize{
\item \eqn{n_q = } \code{nHabitats}, the number of habitats;
\item \eqn{n_p = } \code{nPatches}, the number of patches.
}

If \eqn{w} is any vector describing a quantity in
habitats (\emph{i.e.}, \eqn{\left|w\right|= n_q}), then
\deqn{W={N}\cdot w} is a vector that has summed \eqn{w} by patch, and \eqn{\left|W\right|= n_p}.
}
\examples{
create_habitat_matrix(3, c(1,1,2,2,2))
}
\seealso{
create_habitat matrix is called by \code{\link[=make_xds_template]{make_xds_template()}} and \code{\link[=setup_EGG_LAYING]{setup_EGG_LAYING()}}

see \code{\link[=view_habitat_matrix]{view_habitat_matrix()}}
}
