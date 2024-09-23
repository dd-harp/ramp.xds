
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

#' @title Set up "no LSM"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_lsm <- function(pars) {
  LSM <- 'none'
  class(LSM) <- 'none'
  pars$lsm <- LSM
  return(pars)
}
