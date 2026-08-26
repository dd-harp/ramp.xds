
#' @title Plot the forcing pattern
#'
#' @description Plot the composed time
#' series function forcing a model
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
show_forcing = function(xds_obj, tm = seq(0, 730, by=5), add=FALSE){
  x <- get_mean_forcing(xds_obj)
  season <- F_season(tm, xds_obj)
  trend <- F_trend(tm, xds_obj)
  shock <- F_season(tm, xds_obj)
  ts <- x*season*trend*shock
  if(add==FALSE) plot(tm, ts, ylab = "Forcing", xlab = "Time", type = "n")
  lines(tm, ts)
  return(invisible(ts))
}
