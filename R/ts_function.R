
#' Make a Time Series Function
#' 
#' @description 
#' Build a function that generates a time series
#' with known functions. The value of 
#' variable \eqn{x(t)} is computed
#' as a product of four configurable elements:
#' + \eqn{\bar x}: a mean value 
#' + \eqn{S(t)}: a seasonal pattern 
#' + \eqn{T(t)}: a trend  
#' + \eqn{K(t)}: a shock 
#' 
#' \deqn{x(t) = \bar x \times S(t) \times T(t) \times K(t)} 
#' 
#' The component functions are specified by 
#' passing parameters for [make_function]: 
#' + `season_par` creates \eqn{S(t)} or `F_season` (*eg,* using [makepar_F_sin])
#' + `trend_par` creates \eqn{T(t)} or `F_trend` (*eg,* using [makepar_F_spline])
#' + `shock_par` creates \eqn{K(t)} or `F_shock` (*eg,* using [makepar_F_sharkbite])
#'
#' @param options configurable options
#' @param N the length of the return value
#' @param scale scale parameter, usually the average
#' @param season_par seasonality function for 
#' @param trend_par trend function parameters
#' @param shock_par trend function parameters
#'
#' @return a function
#' @export
make_ts_function = function(options=list(),
                            N=1,
                            scale=1,
                            season_par = list(),
                            trend_par = list(),
                            shock_par = list()){
  with(options,{
    scale = checkIt(scale, N)
    if(length(season_par) == 0)
      season_par = makepar_F_sin(N=N) 
    if(length(trend_par) == 0)
      trend_par = makepar_F_one() 
    if(length(shock_par) == 0)
      shock_par = makepar_F_one() 
 
    F_season = make_function(season_par)   
    F_trend = make_function(trend_par)   
    F_shock = make_function(shock_par)   

    return(function(t, V=list()){scale*F_season(t)*F_trend(t)*F_shock(t)})
  })}