# X

#' @title Solve a Dynamical System
#'
#' @description
#' Besides solving, this function does several housekeeping tasks:
#' + runs [check_models]
#' + sets up the vector of times when outputs are wanted: see [make_times_xde] or [make_times_dts]
#' + sets up \eqn{y_0,} the initial values vector (see [get_inits])
#' + solves the system
#' + parses and attaches outputs (see [parse_outputs])
#'
#' @seealso [xds_setup()]
#' @note
#' The function [xds_solve()] dispatches on `xds_obj$xde`
#'
#' @param xds_obj an **`xds`** model object
#' @param Tmax the last time point, run from 0...Tmax
#' @param dt the time step
#' @param times the times
#'
#' @export
xds_solve = function(xds_obj, Tmax=365, dt=1, times=NULL){
  UseMethod("xds_solve", xds_obj$xde)
}


#' @title Solve a System of Ordinary Differential Equations
#'
#' @description
#' Solves a system of ordinary differential equations and returns
#' the values of the dependent variables.
#'
#' Output times are set by [make_times_xde].
#'
#' The returned model includes parsed outputs.
#'
#' @inheritParams xds_solve
#'
#' @keywords internal
#' @export
xds_solve.ode = function(xds_obj, Tmax=365, dt=1, times=NULL){

  xds_obj <- check_models(xds_obj)

  deSolve::ode(y      = get_inits(xds_obj, flatten=TRUE),
               times  = make_times_xde(Tmax, dt, times),
               func   = xde_derivatives,
               parms  = xds_obj,
               method = "lsoda") -> deout

  xds_obj$outputs$deout <- deout
  tm <- deout[,1]
  xds_obj <- parse_outputs(xds_obj, as.matrix(deout[,-1]), tm)

  return(xds_obj)
}

#' @title Solve a System of Delay Differential Equations
#'
#' @description
#' Solves a system of delay differential equations and computes the values of
#' the dependent variables.
#'
#' Output times are set by [make_times_xde].
#'
#' The returned model includes parsed outputs.
#'
#' @inheritParams xds_solve
#' @keywords internal
#' @export
xds_solve.dde = function(xds_obj, Tmax=365, dt=1, times=NULL){

  xds_obj <- check_models(xds_obj)

  deSolve::dede(y      = get_inits(xds_obj, flatten=TRUE),
                times  = make_times_xde(Tmax, dt, times),
                func   = xde_derivatives,
                parms  = xds_obj,
                method = "lsoda") -> deout

  xds_obj$outputs$deout <- deout
  tm <- deout[,1]
  xds_obj <- parse_outputs(xds_obj, as.matrix(deout[,-1]), tm)

  return(xds_obj)
}

#' @title Solve a Discrete-Time System
#'
#' @description Implements [xds_solve] for discrete time systems.
#' Output times are set up or checked by [make_times_dts] 
#' which guarantees time points are a unique set of integer values
#'
#' @note For discrete time systems, the values in `times` need not be
#' a sequence. The system is solved for every time step. During parsing,
#' the other values are removed. 
#'
#'
#' @inheritParams xds_solve
#' @return an **`xds`** model object
#' @keywords internal
#' @export
xds_solve.dts = function(xds_obj, Tmax=365, dt=1, times=NULL){

  xds_obj <- check_models(xds_obj)
  tm <- make_times_dts(Tmax, dt, times)
  yt = get_inits(xds_obj, flatten=TRUE)
  
  tt <- min(tm):max(tm)
  dts_out = c(tt[1], yt)
  for(t in tt[-1]){
    yt =  unlist(dts_update(t, yt, xds_obj))
    dts_out = rbind(dts_out, c(t,yt))
  }
  dts_out <- dts_out[which(tt %in% tm), ]
  xds_obj$outputs$deout <- dts_out
  
  xds_obj <- parse_outputs(xds_obj, dts_out[, -1], dts_out[,1])
  return(xds_obj)
}


#' @title Times Utility for Differential Equations
#'
#' @description This method sets up the time points for the
#' independent variable to return solutions for the dependent variables.
#'
#' If `times` is not `NULL`, the function returns those values.
#'
#' Otherwise, it returns a sequence from `0` to `Tmax` by `dt`.
#'
#' @param Tmax the last time point, run from 0...Tmax
#' @param dt the time interval for outputs
#' @param times the times
#'
#' @return a vector of time points
#' @keywords internal
#' @export
make_times_xde = function(Tmax, dt, times=NULL){
  stopifnot(dt>0)
  if(length(times)>0) return(times)
  return(seq(0, Tmax, by=dt))
}

#' @title Times Utility for Discrete Time Systems
#'
#' @description This method sets up the time points when output is wanted.
#' For discrete-time systems, values must be computed at every time step, but
#' outputs can be returned only at selected times.
#' 
#' If `times` are provided, unique rounded values are returned.  
#' 
#' Otherwise, it generates a sequence from `0` to `Tmax` by `dt`.
#' 
#' Provided values of `times` are rounded to integers. If `times` is `NULL`,
#' the sequence is rounded and only unique integer values are returned.
#'
#'
#' @param Tmax the last time point, run from 0...Tmax
#' @param dt the time interval for outputs
#' @param times the times
#'
#' @return a vector of time points
#' @keywords internal
#' @export
make_times_dts = function(Tmax=365, dt=1, times=NULL){
  stopifnot(dt>0)
  if(length(times)>0){
    tm = unique(round(times))
  } else {
    tm = seq(0, Tmax, by=dt)
    tm = unique(round(tm))
  } 
  stopifnot("Invalid times vector." = length(tm) > 1)
  
  return(tm)
}
