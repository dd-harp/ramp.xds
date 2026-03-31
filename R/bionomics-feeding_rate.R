#' @title Setup Blood Feeding Rate
#'
#' @description Set up an object
#' to compute dynamic blood feeding rates
#'
#' @param f the blood feeding rate
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
#'
#' @keywords internal
#' @export
setup_f_obj = function(f, MY_obj){
  MY_obj$f = f
  MY_obj$f_t = f
  MY_obj$es_f = 1
  MY_obj$f_obj <- list()
  class(MY_obj$f_obj) <- "static"
  MY_obj$f_obj$f <- f
  return(MY_obj)
}



#' @title Compute the blood feeding rate, f
#'
#' @description  It should
#' set the values of the bionomic parameters to baseline values
#'
#' @note This method dispatches on the type of `f_obj` attached to the `MY_obj`.
#'
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#'
#' @return a [numeric] vector of length `nPatches`
#'
#' @keywords internal
#' @export
F_f = function(t, xds_obj, s) {
  UseMethod("F_f", xds_obj$MY_obj[[s]]$f_obj)
}

#' @title Constant baseline blood feeding rate
#'
#' @description Implements [F_f] for a static model
#'
#' @note This method dispatches on the type of `f_obj` attached to the `MY_obj`.
#'
#' @inheritParams F_f
#'
#' @return \eqn{f}, the baseline blood feeding rate
#' @keywords internal
#' @export
F_f.static = function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$f_obj$f)
}

