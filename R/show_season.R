
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


#' @title Compute the seasonal pattern
#'
#' @description
#' Evaluate the function `F_season` for a forced model.
#'
#' @param tm the time points
#' @param xds_obj an **`xds`** model object
#'
#' @return the seasonal pattern, invisibly
#'
#' @keywords internal
#' @export
F_season = function(tm, xds_obj){
  UseMethod("F_season", xds_obj$forced_by)
}

#' @title Compute the seasonal pattern
#'
#' @description
#' Evaluate the function `F_season` for a forced model when `forced_by` is "none"
#'
#' @inheritParams F_season
#'
#' @return an empty vector
#'
#' @importFrom graphics plot lines
#'
#' @keywords internal
#' @export
F_season.none = function(tm, xds_obj){
  return(c())
}

#' @title Compute the seasonal pattern
#'
#' @description
#' Evaluate the function `F_season` for a forced model when `forced_by` is "Lambda"
#'
#' @inheritParams F_season
#'
#' @return the seasonal pattern, invisibly
#'
#' @importFrom graphics plot lines
#'
#' @keywords internal
#' @export
F_season.Lambda = function(tm, xds_obj){
  return(xds_obj$L_obj[[1]]$F_season(tm))
}

#' @title Compute the seasonal pattern
#'
#' @description
#' Evaluate the function `F_season` for a forced model when `forced_by` is "eir"
#'
#' @inheritParams F_season
#'
#' @return the seasonal pattern, invisibly
#'
#' @keywords internal
#' @export
F_season.eir= function(tm, xds_obj){
  return(xds_obj$EIR_obj$F_season(tm))
}

