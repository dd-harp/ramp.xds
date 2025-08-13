
#' @title Update the seasonality function 
#' 
#' @description Update `F_season` 
#' 
#' @param xds_obj a **`ramp.xds`** model object
#' @param s the vector species index
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_season = function(xds_obj, s=1){
  UseMethod("update_F_season", xds_obj$forced_by)     
}

#' @title Update the seasonality function 
#' 
#' @description Update `F_season` 
#' 
#' @inheritParams update_F_season 
#'  
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_season.Lambda = function(xds_obj, s=1){
  xds_obj$Lpar[[s]]$F_season = make_function(xds_obj$Lpar[[s]]$season_par)
  return(xds_obj)
}

#' @title Update the seasonality function 
#' 
#' @description Update `F_season` 
#' 
#' @inheritParams update_F_season 
#'  
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_season.eir = function(xds_obj, s=1){
  xds_obj$EIRpar$F_season <- make_function(xds_obj$EIRpar$season_par)
  return(xds_obj)
}

#' @title Update the seasonality function 
#' 
#' @description Update `F_season` 
#' 
#' @inheritParams update_F_season 
#'  
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_season.cohort = function(xds_obj, s=1){
  xds_obj$EIRpar$F_season <- make_function(xds_obj$EIRpar$season_par)
  return(xds_obj)
}
