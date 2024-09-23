
#' @title Set the AreaSpray
#' @description Set the value of exogenous variables related to
#' AreaSpray
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
AreaSpray <- function(t, pars) {
  UseMethod("AreaSpray", pars$AreaSpray)
}

#' @title Set no AreaSpray
#' @description The null model for AreaSpray
#' @inheritParams AreaSpray
#' @return [list]
#' @export
AreaSpray.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no AreaSpray"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_area_spray <- function(pars) {
  AreaSpray <- 'none'
  class(AreaSpray) <- 'none'
  pars$area_spray <- AreaSpray
  return(pars)
}
