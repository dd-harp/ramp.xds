
#' @title Setup a Patch Emigration Bionomic Object
#'
#' @description Set up an object
#' to compute the human fraction, \eqn{sigma}
#'
#' @param sigma the mosquito patch emigration rate
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
#'
#' @keywords internal
#' @export
setup_sigma_obj = function(sigma, MY_obj){
  MY_obj$sigma = sigma
  MY_obj$sigma_t = sigma
  MY_obj$es_sigma = 1
  MY_obj$sigma_obj <- list()
  class(MY_obj$sigma_obj) <- "static"
  MY_obj$sigma_obj$sigma <- sigma
  return(MY_obj)
}

#' @title Compute the Mosquito Patch Emigration Rate
#'
#' @description This method dispatches on the type of `sigma_obj`. It should
#' set the values the patch emigration rate, \eqn{\sigma}
#'
#' @inheritParams F_f
#'
#' @return a [numeric] vector osigma length `nPatches`
#'
#' @keywords internal
#' @export
F_sigma = function(t, xds_obj, s){
  UseMethod("F_sigma", xds_obj$MY_obj[[s]]$sigma_obj)
}

#' @title Static model patch emigration
#'
#' @description Implements [F_sigma] for a static model
#'
#' @inheritParams F_sigma
#'
#' @return \eqn{sigma}, the patch emigration rate
#' @keywords internal
#' @export
F_sigma.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$sigma_obj$sigma)
}