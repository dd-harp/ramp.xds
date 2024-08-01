# generic methods for parasite / pathogen importation by visitors

#' @title Visiting
#' @description This method dispatches on the type of `pars$VISITORS`.
#' @param t current simulation time
#' @param pars a [list]
#' @return a [list]
#' @export
Visiting <- function(t, pars) {
  UseMethod("Visiting", pars$VISITORS)
}

#' @title Visiting, a static model
#' @description Implements [Visiting] for the static model (do nothing)
#' @inheritParams Visiting
#' @return a [list]
#' @export
Visiting.static <- function(t, pars) {
  return(pars)
}

#' @title Make parameters for the static model visitors (no visitors)
#' @param pars a [list]
#' @return [list]
#' @export
setup_visitors_static <- function(pars) {

  VISITORS <- list()
  class(VISITORS) <- "static"
  pars$VISITORS <- VISITORS

  return(pars)
}


#' @title Visiting, the basic model
#' @description Implements [Visiting] for the basic model for Visitors
#' @inheritParams Visiting
#' @return a [list]
#' @export
Visiting.basic <- function(t, pars) {
  pars$vars$x_visitors =  with(pars$VISITORS, x_scale*xt(t, pars))
  pars$vars$Visiting =  with(pars$VISITORS, V_scale*Vt(t, pars))
  return(pars)
}

#' @title Make parameters and functions for the basic model for visitors
#' @param pars a [list]
#' @param IMopts a [list]
#' @param x_scale a non-negative numeric value to set the mean for x_visitors
#' @param xt a function to change the pattern for x_visitors over time
#' @param V_scale a non-negative numeric value to set the mean availability of Visiting
#' @param Vt a function to set the temporal pattern for availability of Visiting
#' @return [list]
#' @export
setup_visitors_basic <- function(pars, IMopts, x_scale = 0, xt = NULL, V_scale = 0, Vt = NULL) {with(IMopts,{

  pars$VISITORS$x_scale = x_scale
  if(is.null(xt)) xt = function(t, pars){1}
  pars$VISITORS$xt = xt

  pars$VISITORS$V_scale = V_scale
  if(is.null(Vt)) Vt = function(t, pars){1}
  pars$VISITORS$Vt = Vt

  return(pars)
})}
