

#' @title Setup Laying Rate Bionomic Object
#'
#' @description Set up an object
#' to compute the human fraction, \eqn{nu}
#'
#' @param nu the egg laying rate (# batches, per mosquito, per day)
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
#'
#' @keywords internal
#' @export
setup_nu_obj = function(nu, MY_obj){
  MY_obj$nu = nu
  MY_obj$nu_t = nu
  MY_obj$es_nu = 1
  MY_obj$nu_obj <- list()
  class(MY_obj$nu_obj) <- "static"
  MY_obj$nu_obj$nu <- nu
  return(MY_obj)
}

#' @title Compute the Mosquito Patch Emigration Rate
#'
#' @description This method dispatches on the type of `nu_obj`. It should
#' set the values the patch emigration rate, \eqn{\nu}
#'
#' @inheritParams F_f
#'
#' @return a [numeric] vector onu length `nPatches`
#'
#' @keywords internal
#' @export
F_nu = function(t, xds_obj, s){
  UseMethod("F_nu", xds_obj$MY_obj[[s]]$nu_obj)
}

#' @title Static model patch emigration
#'
#' @description Implements [F_nu] for a static model
#'
#' @inheritParams F_nu
#'
#' @return \eqn{nu}, the patch emigration rate
#' @keywords internal
#' @export
F_nu.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$nu_obj$nu)
}