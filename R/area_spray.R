
#' @title Set the AreaSpray
#' @description Set the value of exogenous variables related to
#' AreaSpray
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
AreaSpray <- function(t, pars) {
  UseMethod("AreaSpray", pars$area_spray)
}

#' @title Set no AreaSpray
#' @description The null model for AreaSpray
#' @inheritParams AreaSpray
#' @return [list]
#' @export
AreaSpray.none <- function(t, pars) {
  return(pars)
}

#' @title Set the AreaSprayEffectSizes
#' @description Set the value of exogenous variables related to
#' AreaSprayEffectSizes
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @param s vector species index
#' @return an **`xds`** object
#' @export
AreaSprayEffectSizes <- function(t, pars, s) {
  UseMethod("AreaSprayEffectSizes", pars$area_spray$effectsizes[[s]])
}

#' @title Set no AreaSprayEffectSizes
#' @description The null model for AreaSprayEffectSizes
#' @inheritParams AreaSprayEffectSizes
#' @return [list]
#' @export
AreaSprayEffectSizes.none <- function(t, pars, s) {
  return(pars)
}

#' @title Set up "no AreaSpray"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_area_spray <- function(pars) {
  AreaSpray <- list()  
  AreaSpray$class <- 'none' 
  class(AreaSpray) <- 'none'
  pars$area_spray <- AreaSpray
  pars$area_spray$effectsizes <- list()  
  pars$area_spray$effectsizes[[1]] <- AreaSpray 
  return(pars)
}
