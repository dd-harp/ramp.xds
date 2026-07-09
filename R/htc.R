#' @title Compute the HTC Matrix
#' 
#' @description Compute the \eqn{N_p \times N_p} 
#' matrix \eqn{[D]}. 
#' 
#'  
#' @param xds_obj a **`ramp.xds`** model object  
#' @param i the host species index
#' @param s the vector species index
#' @param tol a tolerance parameter
#' 
#' @return a numeric [matrix]
#' @export
compute_HTC <- function(xds_obj, i=1, s=1, tol=1e-5){
  D = get_HTC(xds_obj, i)
  y = get_last(xds_obj)
  b = F_infectivity(y, xds_obj, i)
  beta = get_beta(xds_obj, s, i)
  Psi = get_timespent_matrix(xds_obj, i)
  H = get_H(xds_obj, i)
  w = get_search_weights_blood(xds_obj, s, i) 
  htc_matrix = Psi %*% diag(b*D*w*H) %*% beta 
  xds_obj$outputs$HTCmatrix <- htc_matrix  
  return(xds_obj)
}