#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param X a list with new parameters for bottom, phase, and pw 
#' @param xds_obj a **`ramp.xds`** model object
#' @param s the vector species index
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season = function(X, xds_obj, s=1){
  
  if(length(X$bottom) == xds_obj$nPatches)
    xds_obj <- set_season_bottom(X$bottom, xds_obj, s) 
  
  if(length(X$phase) == xds_obj$nPatches)
    xds_obj <- set_season_phase(X$phase, xds_obj, s) 
  
  if(length(X$pw) == xds_obj$nPatches)
    xds_obj <- set_season_pw(X$pw, xds_obj, s)
 
  xds_obj <- update_F_season(xds_obj) 
  
  return(xds_obj)
}


