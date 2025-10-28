
#' @title Plot the Temporal shock 
#' 
#' @description For a model with temporal forcing,
#' show the temporal shock 
#'
#' @param xds_obj an **`xds`** model object
#' @param tm the time points 
#' @param add add to existing plot
#' @param clr the line color 
#' @param rng if not NULL, range limits for plotting
#'
#' @return the temporal shock, invisibly
#'  
#' @export
show_shock = function(xds_obj, tm=10*c(0:365), add=FALSE, clr="black", rng=NULL){
  shock <- F_shock(tm, xds_obj)
  
  if(is.null("rng")) rng = range(shock)
  if(length(shock)>0){
    if(add==FALSE) plot(tm, shock, ylab="Trend", xlab = "Time",
                        col=clr, type="n", ylim=rng)
    lines(tm, shock, col = clr)
  }
  return(invisible(shock)) 
  
}

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
#' @param shock if 0, 
#'
#' @return the temporal trend, invisibly
#'  
#' @export
show_trend = function(xds_obj, tm=10*c(0:365), add=FALSE, clr="black", rng=NULL, shock=TRUE){
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
  return(with(xds_obj$L_obj[[1]], F_trend(tm))) 
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
  return(with(xds_obj$EIR_obj,F_trend(tm)))
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
F_shock = function(tm, xds_obj){
  UseMethod("F_shock", xds_obj$forced_by)
}

#' @title Plot the temporal trend 
#'
#' @description The `F_shock` case for models  
#' with no forcing.
#'
#' @inheritParams F_shock
#'
#' @return a null result 
#' 
#' @importFrom graphics plot lines
#' 
#' @export
F_shock.none = function(tm, xds_obj){
  return(c()) 
}

#' @title Plot the temporal trend 
#'
#' @description For a model forced by emergence,
#' show the temporal trend 
#'
#' @inheritParams F_shock
#'
#' @return the temporal trend
#' 
#' @importFrom graphics plot lines
#' 
#' @export
F_shock.Lambda = function(tm, xds_obj){
  return(with(xds_obj$L_obj[[1]], F_shock(tm))) 
}

#' @title Plot the temporal trend 
#'
#' @description For a model forced by the EIR 
#' plot the temporal trend 
#'
#' @inheritParams F_shock
#'
#' @return the temporal trend, invisibly
#' 
#' @export
F_shock.eir= function(tm, xds_obj){
  return(with(xds_obj$EIR_obj,F_shock(tm)))
}
