

#' @title Plot the Temporal Trend
#'
#' @description For a model with temporal forcing,
#' show the temporal trend
#'
#' @param xds_obj an **`xds`** model object
#' @param tm the time points
#' @param add add to existing plot
#' @param clr the line color
#' @param rng if not NULL, range limits for plotting
#' @param shock if TRUE, compute the trend with a shock 
#'
#' @return the temporal trend, invisibly
#'
#' @export
show_trend = function(xds_obj, tm=10*c(0:365), add=FALSE, clr="black", rng=NULL, shock=FALSE){
  trend <- F_trend(tm, xds_obj)
  if(shock) trend <- trend*F_shock(tm, xds_obj)

  if(is.null("rng")) rng = range(trend)
  if(length(trend)>0){
    if(add==FALSE) plot(tm, trend, ylab="Trend", xlab = "Time",
                        col=clr, type="n", ylim=rng)
    lines(tm, trend, col = clr)
  }
  return(invisible(trend))

}

#' @title Compute the temporal trend
#'
#' @description
#' Evaluate the function `F_trend` for a forced model.
#'
#' @param tm the time points
#' @param xds_obj an **`xds`** model object
#'
#' @return the temporal trend, invisibly
#'
#' @keywords internal
#' @export
F_trend = function(tm, xds_obj){
  UseMethod("F_trend", xds_obj$forced_by)
}

#' @title Compute the temporal trend
#'
#' @description
#' Evaluate the function `F_trend` for a forced model when `forced_by` is "none"
#'
#' @inheritParams F_trend
#'
#' @return an empty vector
#'
#' @importFrom graphics plot lines
#'
#' @keywords internal
#' @export
F_trend.none = function(tm, xds_obj){
  return(c())
}

#' @title Compute the temporal trend
#'
#' @description
#' Evaluate the function `F_trend` for a forced model when `forced_by` is "Lambda"
#'
#' @inheritParams F_trend
#'
#' @return the temporal trend
#'
#' @importFrom graphics plot lines
#'
#' @keywords internal
#' @export
F_trend.Lambda = function(tm, xds_obj){
  return(with(xds_obj$L_obj[[1]], F_trend(tm)))
}

#' @title Compute the temporal trend
#'
#' @description
#' Evaluate the function `F_trend` for a forced model when `forced_by` is "eir"
#'
#' @inheritParams F_trend
#'
#' @return the temporal trend, invisibly
#'
#' @keywords internal
#' @export
F_trend.eir= function(tm, xds_obj){
  return(with(xds_obj$EIR_obj,F_trend(tm)))
}

#' @title Compute the temporal trend
#'
#' @description
#' Evaluate the function `F_trend` for a trivial **MY** model
#'
#' @inheritParams F_trend
#'
#' @return the temporal trend
#'
#' @importFrom graphics plot lines
#'
#' @keywords internal
#' @export
F_trend.MY = function(tm, xds_obj){
  return(with(xds_obj$MY_obj[[1]], F_trend(tm)))
}

#' @title Compute the temporal trend
#'
#' @description
#' Evaluate the function `F_trend` for a trivial **XH** model
#'
#' @inheritParams F_trend
#'
#' @return the temporal trend
#'
#' @importFrom graphics plot lines
#'
#' @keywords internal
#' @export
F_trend.kappa = function(tm, xds_obj){
  return(with(xds_obj$XH_obj[[1]], F_trend(tm)))
}