
#' @title Set the LSM
#' @description Set the value of exogenous variables related to
#' LSM
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
LSM <- function(t, pars) {
  UseMethod("LSM", pars$lsm)
}

#' @title Set no LSM
#' @description The null model for LSM
#' @inheritParams LSM
#' @return an **`xds`** object
#' @export
LSM.none <- function(t, pars) {
  return(pars)
}

#' @title Set up LSM effect sizes
#' @description Set up effect sizes for LSM
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @param s vector species index
#' @return an **`xds`** object
#' @export
LSMEffectSizes <- function(t, pars, s) {
  UseMethod("LSMEffectSizes", pars$lsm$effectsizes[[s]])
}

#' @title Set no LSMEffectSizes
#' @description The null model for LSMEffectSizes
#' @inheritParams LSMEffectSizes
#' @return an **`xds`** object
#' @export
LSMEffectSizes.none <- function(t, pars, s) {
  return(pars)
}


#' @title Set up "no LSM"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_lsm <- function(pars) {
  LSM <- list()  
  LSM$class <- 'none'
  class(LSM) <- 'none'
  pars$lsm <- LSM
  pars$lsm$effectsizes <- list()
  pars$lsm$effectsizes[[1]] <- LSM
  return(pars)
}
