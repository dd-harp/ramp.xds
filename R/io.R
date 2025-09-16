
#' Shrink an xds xds_obj object
#'
#' @param xds_obj an xds xds_obj object
#'
#' @returns invisible() 
#' @export
xds_shrink = function(xds_obj){
  UseMethod("xds_shrink", xds_obj$frame)
}


#' Shrink an xds xds_obj object
#'
#' @param xds_obj an xds xds_obj object
#'
#' @returns a smaller xds xds_obj object 
#' @export
xds_shrink.cohort = function(xds_obj){
  xds_obj$F_eir <- list()  
  xds_obj$EIRpar$F_season <- list()  
  xds_obj$EIRpar$F_trend <- list()  
  xds_obj$EIRpar$F_age <- list()  
  xds_obj$outputs <- list()  
  return(xds_obj) 
}

