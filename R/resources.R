# Methods to compute resource dynamics and availability

#' @title Set the values of exogenous variables describing available mosquito resources
#' @description This method dispatches on the type of `pars$RESOURCES`.
#' @param t current simulation time
#' @param pars a [list]
#' @return none
#' @export
Resources <- function(t, pars) {
  UseMethod("Resources", pars$RESOURCES)
}

#' @title Modify resources and resource availability
#' @description Implements [Resources] for the static model of resources
#' @inheritParams Resources
#' @return none
#' @export
Resources.static <- function(t, pars) {
  return(pars)
}

#' @title Set up parameters for the static model for resource availability
#' @param pars a [list]
#' @return none
#' @export
setup_resources_static <- function(pars){
  RESOURCES <- list()
  class(RESOURCES) <- 'static'
  pars$RESOURCES <- RESOURCES
  
  return(pars)
}
