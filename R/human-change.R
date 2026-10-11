
#' @title Change human population density
#' @param H human population density
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [list]
#' @keywords internal
#' @export
change_H = function(H, xds_obj, i=1){
  stopifnot(length(H) == xds_obj$nStrata[i])
  vars <- get_XH_inits(xds_obj,i)
  vars$H <- H
  xds_obj <- change_XH_inits(xds_obj, i, vars)
  xds_obj <- update_XY_interface(xds_obj)
  return(xds_obj)
}
