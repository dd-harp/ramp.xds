
#' @title Get xi, delayed maturation 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param i the host species index
#' @return the mixing matrix, beta
#' @export
get_beta = function(xds_obj, s=1, i=1){
  xds_obj$terms$beta[[s]][[i]]
}