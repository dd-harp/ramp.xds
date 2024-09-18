
#' @title Set the humidity
#' @description Set the value of exogenous variables related to
#' humidity
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Humidity <- function(t, pars) {
  UseMethod("Humidity", pars$humidity)
}

#' @title Set no humidity
#' @description The null model for humidity
#' @inheritParams Humidity
#' @return [list]
#' @export
Humidity.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no humidity"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_humidity <- function(pars) {
  humidity <- 'none'
  class(humidity) <- 'none'
  pars$humidity <- humidity
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
setup_humidity = function(name, pars, Topts=list()){
  class(name) <- name
  UseMethod("setup_humidity", name)
}

#' @title Set no humidity
#' @description The null model for humidity
#' @inheritParams Humidity
#' @return [list]
#' @export
Humidity.func <- function(t, pars) {with(pars$humidity,{
  pars$vars$Humidity = mean*F_season(t)*F_trend(t)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_humidity
#' @export
setup_humidity.func = function(name, pars, Topts=list()){
  pars = setup_humidity_func(pars, Topts())
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param Topts a list of options to override defaults
#' @param mean the mean humidity
#' @param F_season the seasonal signal in humidity
#' @param F_trend a temporal trend in humidity
#' @return an **`xds`** object
#' @export
setup_humidity_func = function(pars, Topts=list(), mean = 80, F_season=F_flat, F_trend=F_flat){
  humidity <- list()
  class(humidity) <- 'func'
  humidity$meanT <- mean
  humidity$F_season <- F_season
  humidity$F_trend <- F_trend
  pars$humidity <- humidity
  return(pars)
}

