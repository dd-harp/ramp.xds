
#' @title A trivial function F_flat
#' @description A function that returns 1
#' @param x an arbitrary input
#' @return a vector of ones of length x
#' @export
F_flat = function(x){return(0*x+1)}

#' @title A trivial function F_no_season
#' @description A function that returns 1
#' @param t the time
#' @param phase the phase
#' @param season_opts a list of options
#' @return a vector of ones of length x
#' @export
F_no_season = function(t, phase=0, season_opts=list){return(0*t+1)}

#' @title A trivial function F_no_season
#' @description A function that returns 1
#' @param t the time
#' @param phase the phase
#' @param season_opts a list of options
#' @return a vector of ones of length x
#' @export
F_sin_season = function(t, phase, season_opts=list(pwr=1)){
  return(with(season_opts,
              (1+sin(2*pi*(t+phase)/365))^pwr))}

#' @title A trivial function F_no_trend
#' @description A function that returns 1
#' @param t the time
#' @param trend_opts a list of options
#' @return a vector of ones of length x
#' @export
F_no_trend = function(t, trend_opts=list()){return(0*t+1)}
