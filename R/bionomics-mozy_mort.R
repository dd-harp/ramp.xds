
#' @title Setup a Mosquito Mortality Bionomic Object
#'
#' @description Set up an object to return a
#' constant baseline mosquito mortality rate, \eqn{g}
#'
#' @param g the mosquito mortality rate
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
#'
#' @export
setup_mozy_mort = function(g, MY_obj){
  MY_obj$g = g
  MY_obj$g_t = g
  MY_obj$es_g = 1
  MY_obj$g_obj <- list()
  class(MY_obj$g_obj) <- "static"
  MY_obj$g_obj$g <- g
  return(MY_obj)
}

#' @title Compute the blood geeding rate, g
#'
#' @description This method dispatches on the type of `g_obj`. It should
#' set the values og the bionomic parameters to baseline values
#'
#' @inheritParams F_feeding_rate
#'
#' @return a [numeric] vector og length `nPatches`
#'
#' @keywords internal
#' @export
F_mozy_mort = function(t, xds_obj, s) {
  UseMethod("F_mozy_mort", xds_obj$MY_obj[[s]]$g_obj)
}

#' @title Static model for the blood feeding rate
#' @description Implements [F_mozy_mort] for a static model
#' @inheritParams F_mozy_mort
#' @return \eqn{g}, the baseline human fraction
#' @keywords internal
#' @export
F_mozy_mort.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$g_obj$g)
}
