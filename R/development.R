
#' @title Set the development
#' @description Set the value of exogenous variables related to
#' development
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Development <- function(t, pars) {
  UseMethod("Development", pars$forcing$development)
}

#' @title Set no development
#' @description The null model for development
#' @inheritParams Development
#' @return [list]
#' @export
Development.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no development"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_development <- function(pars) {
  development <- 'none'
  class(development) <- 'none'
  pars$forcing$development <- development
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param Topts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_development = function(name, pars, Topts=list()){
  class(name) <- name
  UseMethod("setup_development", name)
}

#' @title Set no development
#' @description The null model for development
#' @inheritParams Development
#' @return [list]
#' @export
Development.func <- function(t, pars) {with(pars$development,{
  pars$vars$housing_quality = mean*F_season(t)*F_trend(t)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_development
#' @export
setup_development.func = function(name, pars, Topts=list()){
  pars <- dynamic_forcing(pars)
  pars = setup_development_func(pars, Topts())
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param Topts a list of options to override defaults
#' @param mean the mean water level
#' @param F_season the seasonal signal in development
#' @param F_trend a temporal trend in development
#' @return an **`xds`** object
#' @export
setup_development_func = function(pars, Topts=list(), mean=30, F_season=F_flat, F_trend=F_flat){
  development <- list()
  class(development) <- 'func'
  development$mean <- mean
  development$F_season <- F_season
  development$F_trend <- F_trend
  pars$development <- development
  return(pars)
}

