
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

#' @title Set up no sugar baits
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_sugar_baits <- function(pars) {
  SugarBaits <- 'none'
  class(SugarBaits) <- 'none'
  pars$sugar_baits <- SugarBaits
  return(pars)
}
