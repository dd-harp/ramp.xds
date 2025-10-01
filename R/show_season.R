
#' @title Plot the seasonal pattern 
#'
#' @description For a model forced by emergence,
#' show the seasonal pattern 
#'
#' @param xds_obj an **`xds`** model object
#' @param tm the time points 
#' @param add add to existing plot
#' 
#' @return the seasonal pattern, invisibly 
#' 
#' @importFrom graphics plot lines
#' 
#' @export
show_season= function(xds_obj, tm = seq(0, 730, by=5), add=FALSE){
  season <- F_season(tm, xds_obj) 
  if(add==FALSE) plot(tm, season, ylab = "Seasonal Pattern", xlab = "Time", type = "n")
  lines(tm, season)
  return(invisible(season))
}


#' @title Plot the seasonal pattern 
#'
#' @param tm the time points 
#' @param xds_obj an **`xds`** model object
#'
#' @return the seasonal pattern, invisibly
#'  
#' @export
F_season = function(tm, xds_obj){
  UseMethod("F_season", xds_obj$forced_by)
}

#' @title Plot the seasonal pattern 
#'
#' @description The `F_season` case for models  
#' with no forcing.
#'
#' @inheritParams F_season
#'
#' @return a null result 
#' 
#' @importFrom graphics plot lines
#' 
#' @export
F_season.none = function(tm, xds_obj){
  return(c()) 
}

#' @title Plot the seasonal pattern 
#'
#' @description For a model forced by emergence,
#' show the seasonal pattern 
#'
#' @inheritParams F_season
#'
#' @return the seasonal pattern, invisibly 
#' 
#' @importFrom graphics plot lines
#' 
#' @export
F_season.Lambda = function(tm, xds_obj){
  return(xds_obj$L_obj[[1]]$F_season(tm))
}

#' @title Plot the seasonal pattern 
#'
#' @description For a model forced by the EIR 
#' plot the seasonal pattern 
#'
#' @inheritParams F_season
#'
#' @return the seasonal pattern, invisibly
#' 
#' @export
F_season.eir= function(tm, xds_obj){
  return(xds_obj$EIR_obj$F_season(tm)) 
}

