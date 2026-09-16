#' @title Check circadian
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(circadian) == nPatc
#' 
#' 
#' @param F_circadian a circadian pattern function 
#' @param tol a tolerance function
#' 
#' @importFrom stats integrate
#' @seealso [xds_info_blood_feeding]
#' @export
check_F_circadian = function(F_circadian, tol=1e-5){
  d = seq(0, 1, by=0.01)
  stopifnot(F_circadian(d)>0)
  stopifnot(abs(integrate(F_circadian, 0, 1)$value-1)< tol)
}

#' @title Show Circadian
#' 
#' @description
#' Update the circadian for the \eqn{s^{th}} host species
#' 
#' @param F_circadian a circadian function
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' 
#' @export
setup_F_circadian = function(F_circadian, xds_obj, s=1){
  check_F_circadian(F_circadian)
  xds_obj$MY_obj[[s]]$F_circadian <- F_circadian
  return(xds_obj)
}

#' @title Get availability of circadian
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [matrix]
#' @export
show_F_circadian = function(xds_obj, s=1){
  d <- seq(0, 1, length.out=100)
  plot(d, xds_obj$MY_obj[[s]]$F_circadian(d), type = "l")
  
}


