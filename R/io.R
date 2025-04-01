
#' Shrink an xds model object
#'
#' @param model an xds model object
#'
#' @returns invisible() 
#' @export
xds_shrink = function(model){
  UseMethod("xds_shrink", model$frame)
}


#' Shrink an xds model object
#'
#' @param model an xds model object
#'
#' @returns a smaller xds model object 
#' @export
xds_shrink.cohort = function(model){
  model$F_eir <- list()  
  model$EIRpar$F_season <- list()  
  model$EIRpar$F_trend <- list()  
  model$EIRpar$F_age <- list()  
  model$outputs <- list()  
  return(model) 
}

