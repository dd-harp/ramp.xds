
#' @title Set the rainfall
#' @description Set the value of exogenous variables related to
#' rainfall
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Rainfall <- function(t, pars) {
  UseMethod("Rainfall", pars$forcing$weather$rainfall)
}

#' @title Set no rainfall
#' @description The null model for rainfall
#' @inheritParams Rainfall
#' @return [list]
#' @export
Rainfall.basic <- function(t, pars) {
  return(pars)
}

#' @title Set up "no rainfall"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_rainfall <- function(pars) {
  rainfall <- 'basic'
  class(rainfall) <- 'basic'
  pars$forcing$weather$rainfall <- rainfall
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param Tname the name of a model to set up
#' @param pars an **`xds`** object
#' @param Topts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_rainfall = function(Tname, pars, Topts=list()){
  class(Tname) <- Tname
  UseMethod("setup_rainfall", Tname)
}

#' @title Set no rainfall
#' @description The null model for rainfall
#' @inheritParams Rainfall
#' @return [list]
#' @export
Rainfall.func <- function(t, pars) {with(pars$rainfall,{
  pars$vars$Rainfall = mean*F_season(t)*F_trend(t)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_rainfall
#' @export
setup_rainfall.func = function(Tname, pars, Topts=list()){
  pars = setup_rainfall_sin(pars, Topts())
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param Topts a list of options to override defaults
#' @param mean the mean rainfall
#' @param F_season the seasonal signal in rainfall
#' @param F_trend a temporal trend in rainfall
#' @return an **`xds`** object
#' @export
setup_rainfall_func = function(pars, Topts=list(), mean=30, F_season=F_flat, F_trend=F_flat){
  rainfall <- list()
  class(rainfall) <- 'func'
  rainfall$meanT <- mean
  rainfall$F_season <- F_season
  rainfall$F_trend <- F_trend
  pars$rainfall <- rainfall
  return(pars)
}

