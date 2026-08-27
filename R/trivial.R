#' @title Forcing with Trivial Modules
#' 
#' @description
#' Trivial modules were developed for each one
#' of the three dynamical components, making it 
#' possible to develop studies of some focal process
#' with known inputs: a *trace function* approach.
#' 
#' Three trivial modules and EIR forcing (see [xds_setup_eir])
#' construct trace functions as composed time series functions. The 
#' value of a forced variable \eqn{x(t)} is computed
#' as a product of four configurable elements:
#' + \eqn{\bar x}: a mean value 
#' + \eqn{F_S(t, V)}: a seasonal pattern 
#' + \eqn{F_T(t, V)}: a trend 
#' + \eqn{F_K(t, V)}: a shock 
#' 
#' \deqn{x(t) = \bar x \times F_S(t, V) \times F_T(t,V) \times F_K(t, V)} 
#' 
#' + A trace function library for RAMP is in `ramp.func`
#' 
#' + Functions that integrate `ramp.func` and `ramp.xds` are found in `ramp.forcing`
#'
#' For a discussion of mechanistic approaches to forcing, see [SimBA: Trace Functions](https://faculty.washington.edu/smitdave/simba/trace.html)
#' 
#' @seealso [trivial_XH], [trivial_MY], [trivial_L], [xds_setup_eir]
#' @name xds_info_trivial_forcing
NULL
