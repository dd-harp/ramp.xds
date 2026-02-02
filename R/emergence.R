#' @title Compute emerging adults
#' @description
#' Sums up adults emerging from all aquatic habitats using the habitat membership matrix
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @return an `xds` object
#' @export
#' @keywords internal
Emergence = function(t, y, xds_obj){
  N = xds_obj$ML_interface$habitat_matrix
  for(s in 1:xds_obj$nVectorSpecies)
    alpha = F_emerge(t, y, xds_obj, s)
    xds_obj$terms$alpha[[s]] = alpha 
    xds_obj$terms$Lambda[[s]] = N %*% alpha 
  return(xds_obj)
}
