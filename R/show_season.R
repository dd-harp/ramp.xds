
#' @title Plot the seasonal pattern 
#'
#' @param xds_obj a **`ramp.xds** model object
#' @param tm the time points 
#' @param add add to existing plot
#'
#' @return the seasonal pattern, invisibly
#'  
#' @export
show_season = function(xds_obj, tm = seq(0, 730, by=5), add=FALSE){
  UseMethod("show_season", xds_obj$forced_by)
}

#' @title Plot the seasonal pattern 
#'
#' @description The `show_season` case for models  
#' with no forcing.
#'
#' @inheritParams show_season
#'
#' @return a null result 
#' 
#' @importFrom graphics plot lines
#' 
#' @export
show_season.none = function(xds_obj, tm = seq(0, 730, by=5), add=FALSE){
  return(c()) 
}

#' @title Plot the seasonal pattern 
#'
#' @description For a model forced by emergence,
#' show the seasonal pattern 
#'
#' @inheritParams show_season
#'
#' @return the seasonal pattern, invisibly 
#' 
#' @importFrom graphics plot lines
#' 
#' @export
show_season.Lambda = function(xds_obj, tm = seq(0, 730, by=5), add=FALSE){
  season <- xds_obj$Lpar[[1]]$F_season(tm)
  if(add==FALSE) plot(tm, season, ylab = "Seasonal Pattern", xlab = "Time", type = "n")
  lines(tm, season)
  return(invisible(season))
}

#' @title Plot the seasonal pattern 
#'
#' @description For a model forced by the EIR 
#' plot the seasonal pattern 
#'
#' @inheritParams show_season
#'
#' @return the seasonal pattern, invisibly
#' 
#' @export
show_season.eir= function(xds_obj, tm = seq(0, 730, by=5), add=FALSE){
  season <- xds_obj$EIRpar$F_season(tm)
  if(add==FALSE) plot(tm, season, ylab = "Seasonal Pattern", xlab = "Time", type = "n")
  lines(tm, season)
  return(invisible(season))
}

