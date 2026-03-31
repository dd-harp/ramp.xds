
#' @title Setup a Dispersal Loss Bionomic Object
#'
#' @description Set up an object
#' to compute the dispersal loss fraction, \eqn{\mu}
#'
#' @param mu the emigration loss fraction
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
#'
#' @keywords internal
#' @export
setup_mu_obj = function(mu, MY_obj){
  MY_obj$mu = mu
  MY_obj$mu_t = mu
  MY_obj$es_mu = 1
  MY_obj$mu_obj <- list()
  class(MY_obj$mu_obj) <- "static"
  MY_obj$mu_obj$mu <- mu
  return(MY_obj)
}

#' @title Compute the emigration loss fraction
#'
#' @description This method dispatches on the type of `mu_obj`. It should
#' set the values omu the bionomic parameters to baseline values
#'
#' @inheritParams F_f
#'
#' @return a [numeric] vector omu length `nPatches`
#'
#' @keywords internal
#' @export
F_mu <- function(t, xds_obj, s){
  UseMethod("F_mu", xds_obj$MY_obj[[s]]$mu_obj)
}

#' @title Static model for the blood feeding rate
#' @description Implements [F_mu] for a static model
#' @inheritParams F_mu
#' @return \eqn{mu}, the baseline human fraction
#' @keywords internal
#' @export
F_mu.static <- function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$mu_obj$mu)
}
