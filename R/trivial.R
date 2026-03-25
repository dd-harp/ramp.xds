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
#' In the trivial modules, the functions are specified by 
#' passing parameters generated for [make_function]. 
#' + `season_par` creates \eqn{S(t)} or `F_season` (*eg,* using [makepar_F_sin])
#' + `trend_par` creates \eqn{T(t)} or `F_trend` (*eg,* using [makepar_F_spline])
#' + `shock_par` creates \eqn{K(t)} or `F_shock` (*eg,* using [makepar_F_sharkbite])
#'
#' For a discussion of mechanistic approaches to forcing, see [Forcing]
#' 
#' @seealso [trivial_XH], [trivial_MY], [trivial_L], [xds_setup_eir], [make_ts_function], and [Forcing]
#' @name xds_info_trivial_forcing
NULL
