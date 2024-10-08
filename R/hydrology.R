
#' @title Set the hydrology
#' @description Set the value of exogenous variables related to
#' hydrology
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Hydrology <- function(t, pars) {
  UseMethod("Hydrology", pars$hydrology)
}

#' @title Set no hydrology
#' @description The null model for hydrology
#' @inheritParams Hydrology
#' @return [list]
#' @export
Hydrology.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no hydrology"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_hydrology <- function(pars) {
  hydrology <- list()
  hydrology$name <- 'none'
  class(hydrology) <- 'none'
  pars$hydrology <- hydrology
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param Hname the name of a model to set up
#' @param pars an **`xds`** object
#' @param setup_no_forcing a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_hydrology = function(Hname, pars, setup_no_forcing=list()){
  class(Hname) <- Hname
  UseMethod("setup_hydrology", Hname)
}

#' @title Set no hydrology
#' @description The null model for hydrology
#' @inheritParams Hydrology
#' @return [list]
#' @export
Hydrology.func <- function(t, pars) {with(pars$hydrology,{
  pars$vars$water_level = mean*F_season(t)*F_trend(t)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_hydrology
#' @export
setup_hydrology.func = function(Hname, pars, setup_no_forcing=list()){
  pars = setup_hydrology_func(pars, setup_no_forcing())
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param setup_no_forcing a list of options to override defaults
#' @param mean the mean water level
#' @param F_season the seasonal signal in hydrology
#' @param F_trend a temporal trend in hydrology
#' @return an **`xds`** object
#' @export
setup_hydrology_func = function(pars, setup_no_forcing=list(), mean=30, F_season=F_flat, F_trend=F_flat){
  hydrology <- list()
  class(hydrology) <- 'func'
  hydrology$mean <- mean
  hydrology$F_season <- F_season
  hydrology$F_trend <- F_trend
  pars$hydrology <- hydrology
  return(pars)
}

