
#' @title Plot the temporal trend 
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
  UseMethod("show_trend", xds_obj$forced_by)
}

#' @title Plot the temporal trend 
#'
#' @description The `show_trend` case for models  
#' with no forcing.
#'
#' @inheritParams show_trend
#'
#' @return a null result 
#' 
#' @importFrom graphics plot lines
#' 
#' @export
show_trend.none = function(xds_obj, tm = seq(0, 3650, by=10), add=FALSE){
  return(c()) 
}

#' @title Plot the temporal trend 
#'
#' @description For a model forced by emergence,
#' show the temporal trend 
#'
#' @inheritParams show_trend
#'
#' @return the temporal trend, invisibly 
#' 
#' @importFrom graphics plot lines
#' 
#' @export
show_trend.Lambda = function(xds_obj, tm = seq(0, 3650, by=10), add=FALSE){
  trend <- xds_obj$Lpar[[1]]$F_trend(tm)
  if(add==FALSE) plot(tm, trend, ylab = "Temporal Trend", xlab = "Time", type = "n")
  lines(tm, trend)
  return(invisible(trend))
}

#' @title Plot the temporal trend 
#'
#' @description For a model forced by the EIR 
#' plot the temporal trend 
#'
#' @inheritParams show_trend
#'
#' @return the temporal trend, invisibly
#' 
#' @export
show_trend.eir= function(xds_obj, tm = seq(0, 3650, by=10), add=FALSE){
  trend <- xds_obj$EIRpar$F_trend(tm)
  if(add==FALSE) plot(tm, trend, ylab = "Temporal Trend", xlab = "Time", type = "n")
  lines(tm, trend)
  return(invisible(trend))
}
