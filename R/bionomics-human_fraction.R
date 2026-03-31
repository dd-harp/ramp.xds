
#' @title Setup a Human Fraction Bionomic Object
#'
#' @description Set up an object
#' to compute the human fraction, \eqn{q}
#'
#' @param q the human fraction
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
#'
#' @keywords internal
#' @export
setup_q_obj = function(q, MY_obj){
  MY_obj$q = q
  MY_obj$q_t = q
  MY_obj$es_q = 1
  MY_obj$q_obj <- list()
  class(MY_obj$q_obj) <- "static"
  MY_obj$q_obj$q <- q
  return(MY_obj)
}

#' @title Compute the blood qeeding rate, q
#'
#' @description This method dispatches on the type of `q_obj`. It should
#' set the values oq the bionomic parameters to baseline values
#'
#' @inheritParams F_f
#'
#' @return a [numeric] vector oq length `nPatches`
#'
#' @keywords internal
#' @export
F_q = function(t, xds_obj, s) {
  UseMethod("F_q", xds_obj$MY_obj[[s]]$q_obj)
}

#' @title Static model for the blood feeding rate
#' @description Implements [F_q] for a static model
#' @inheritParams F_q
#' @return \eqn{q}, the baseline human fraction
#' @keywords internal
#' @export
F_q.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$q_obj$q)
}
