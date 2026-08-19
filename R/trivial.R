#' @title Forcing with Trivial Modules
#' 
#' @description
#' Trivial modules were developed for each one
#' of the three dynamical components, making it 
#' possible to develop studies of some focal process
#' with known inputs: a *trace function* approach.
#' 
#' Three trivial modules and EIR forcing [xds_setup_eir]
#' construct trace functions as decomposable time series. The 
#' value of a forced variable \eqn{x(t)} is computed
#' as a product of four configurable elements:
#' + \eqn{\bar x}: a mean value 
#' + \eqn{S(t)}: a seasonal pattern 
#' + \eqn{T(t)}: a trend 
#' + \eqn{K(t)}: a shock 
#' 
#' \deqn{x(t) = \bar x \times S(t) \times T(t) \times K(t)} 
#' 
#' + A trace function library for RAMP is in `ramp.func`
#' 
#' + Functions that integrate `ramp.func` and `ramp.xds` are found in `ramp.forcing`
#'
#' For a discussion of mechanistic approaches to forcing, see [Forcing]
#' 
#' @seealso [trivial_XH], [trivial_MY], [trivial_L], [xds_setup_eir], and [Forcing]
#' @name xds_info_trivial_forcing
NULL
