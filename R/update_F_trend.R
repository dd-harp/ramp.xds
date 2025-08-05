
#' @title Update the trend function 
#' 
#' @description Update `F_trend`
#' 
#' @param xds_obj a **`ramp.xds`** model object
#' @param s the vector species index
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_trend = function(xds_obj, s=1){
  UseMethod("update_F_trend", xds_obj$forced_by)     
}

#' @title Update the trend function 
#' 
#' @description Update `F_trend`
#' 
#' @inheritParams update_F_trend 
#'  
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_trend.Lambda = function(xds_obj, s=1){
  xds_obj$Lpar[[1]]$F_trend = make_function(xds_obj$Lpar[[1]]$trend_par)
  return(xds_obj)
}

#' @title Update the trend function 
#' 
#' @description Update `F_trend`
#' 
#' @param xds_obj a **`ramp.xds`** model object
#' @param s the vector species index
#'  
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_trend.eir = function(xds_obj, s=1){
  set_eir(xds_obj)
}

#' @title Update the trend function 
#' 
#' @description Update `F_trend`
#' 
#' @param xds_obj a **`ramp.xds`** model object
#' @param s the vector species index
#'  
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_trend.cohort = function(xds_obj, s=1){
  set_eir(xds_obj)
}
