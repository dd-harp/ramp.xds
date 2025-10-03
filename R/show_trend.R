

#' @title Plot the Temporal Trend 
#' 
#' @description For a model with temporal forcing,
#' show the temporal trend 
#'
#' @param xds_obj an **`xds`** model object
#' @param tm the time points 
#' @param add add to existing plot
#'
#' @return the temporal trend, invisibly
#'  
#' @export
show_trend = function(xds_obj, tm = seq(0, 3650, by=10), add=FALSE){
  trend <- F_trend(tm, xds_obj)
  if(length(trend)>0){
    if(add==FALSE) plot(tm, trend, ylab = "Temporal Trend", xlab = "Time", type = "n")
    lines(tm, trend)
  }
  return(invisible(trend)) 
  
}

#' @title Plot the Temporal Trend 
#' 
#' @description For a model with temporal forcing,
#' show the temporal trend 
#'
#' @param tm the time points 
#' @param xds_obj an **`xds`** model object
#'
#' @return the temporal trend, invisibly
#'  
#' @export
F_trend = function(tm, xds_obj){
  UseMethod("F_trend", xds_obj$forced_by)
}

#' @title Plot the temporal trend 
#'
#' @description The `F_trend` case for models  
#' with no forcing.
#'
#' @inheritParams F_trend
#'
#' @return a null result 
#' 
#' @importFrom graphics plot lines
#' 
#' @export
F_trend.none = function(tm, xds_obj){
  return(c()) 
}

#' @title Plot the temporal trend 
#'
#' @description For a model forced by emergence,
#' show the temporal trend 
#'
#' @inheritParams F_trend
#'
#' @return the temporal trend
#' 
#' @importFrom graphics plot lines
#' 
#' @export
F_trend.Lambda = function(tm, xds_obj){
  return(with(xds_obj$L_obj[[1]], F_trend(tm)*F_shock(tm))) 
}

#' @title Plot the temporal trend 
#'
#' @description For a model forced by the EIR 
#' plot the temporal trend 
#'
#' @inheritParams F_trend
#'
#' @return the temporal trend, invisibly
#' 
#' @export
F_trend.eir= function(tm, xds_obj){
  return(with(xds_obj$EIR_obj,F_trend(tm)*F_shock(tm)))
}
