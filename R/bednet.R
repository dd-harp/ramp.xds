
#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
BedNet <- function(t, pars) {
  UseMethod("BedNet", pars$bednets)
}

#' @title Set no bednet
#' @description The null model for bednet
#' @inheritParams BedNet
#' @return [list]
#' @export
BedNet.none <- function(t, pars) {
  return(pars)
}


#' @title Set the bednet_effectsizes
#' @description Set the value of exogenous variables related to
#' bednet_effectsizes
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return an **`xds`** object
#' @export
BedNetEffectSizes <- function(t, pars, s) {
  UseMethod("BedNetEffectSizes", pars$bednets$effectsizes)
}


#' @title Set no bednet_effectsizes
#' @description The null model for bednet_effectsizes
#' @inheritParams BedNetEffectSizes
#' @return [list]
#' @export
BedNetEffectSizes.none <- function(t, pars, s) {
  return(pars)
}

#' @title Set up "no bednet"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_bednets <- function(pars) {
  bednets <- list()
  bednets$name <- 'none'
  class(bednets) <- 'none'
  pars$bednets <- bednets
  pars$bednets$effectsizes <- list()  
  pars$bednets$effectsizes[[1]] <- bednets
  return(pars)
}

