
#' @title Set the SugarBaits
#' @description Set the value of exogenous variables related to
#' Sugar Baits
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
SugarBaits <- function(t, pars) {
  UseMethod("SugarBaits", pars$sugar_baits)
}

#' @title No sugar baits
#' @description The null model for sugar baits
#' @inheritParams SugarBaits
#' @return [list]
#' @export
SugarBaits.none <- function(t, pars) {
  return(pars)
}

#' @title Set the SugarBaitEffectSizes
#' @description Set the value of exogenous variables related to
#' Sugar Baits
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @param s vector species index
#' @return an **`xds`** object
#' @export
SugarBaitEffectSizes <- function(t, pars, s) {
  UseMethod("SugarBaitEffectSizes", pars$sugar_baits$effectsizes[[s]])
}

#' @title No sugar baits
#' @description The null model for sugar baits
#' @inheritParams SugarBaitEffectSizes
#' @return [list]
#' @export
SugarBaitEffectSizes.none <- function(t, pars, s) {
  return(pars)
}

#' @title Set up no sugar baits
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_sugar_baits <- function(pars) {
  SugarBaits <- list()  
  class(SugarBaits) <- 'none'
  pars$sugar_baits <- SugarBaits
  pars$sugar_baits$effectsizes <- list()  
  pars$sugar_baits$effectsizes[[1]] <- SugarBaits 
  return(pars)
}
