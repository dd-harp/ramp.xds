
#' @title Implement IRS
#' @description Set the value of exogenous variables related to
#' IRS
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
IRS <- function(t, pars) {
  UseMethod("IRS", pars$irs)
}

#' @title Set no irs
#' @description The null model for irs
#' @inheritParams IRS
#' @return an **`xds`** object
#' @export
IRS.none <- function(t, pars) {
  return(pars)
}

#' @title Set the irs_effectsizes
#' @description Set the value of exogenous variables related to
#' irs_effectsizes
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @param s vector species index
#' @return an **`xds`** object
#' @export
IRSEffectSizes <- function(t, pars, s) {
  UseMethod("IRSEffectSizes", pars$irs$effectsizes[[s]])
}

#' @title Set no irs_effectsizes
#' @description The null model for irs_effectsizes
#' @inheritParams IRSEffectSizes
#' @return an **`xds`** object
#' @export
IRSEffectSizes.none <- function(t, pars, s) {
  return(pars)
}

#' @title Set up "no irs"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_irs <- function(pars) {
  irs <- list()
  class(irs) = 'none'
  irs$name = 'none'
  pars$irs <- irs
  pars$irs$effectsizes <- list()   
  pars$irs$effectsizes[[1]] <- irs 
  return(pars)
}
