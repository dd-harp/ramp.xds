# Defines xds_solve and make_times 

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
#' @note 
#' The function [xds_solve()] dispatches on `xds_obj$xde`
#'
#' @seealso [xde_derivatives()] or [dts_update()]
#' 
#' @param xds_obj an **`xds`** model object
#' @param Tmax the last time point, run from 0...Tmax
#' @param dt the time step 
#' @param times the times
#' 
#' @return an **`xds`** object
#' @export
xds_solve = function(xds_obj, Tmax=365, dt=1, times=NULL){
  UseMethod("xds_solve", xds_obj$xde)
}


#' @title Solve a System of Ordinary Differential Equations
#' 
#' @description This method solves a system of equations and computes the values of
#' the dependent variables at times \eqn{t=0, dt, 2dt, ... Tmax}. The function dispatches
#' on `xds_obj$dlay` to call:
#'
#'   - [deSolve::ode] if `class(dlay) == 'ode'`
#'   - [deSolve::dede] if `class(dlay) == 'dde'`
#'
#' Note that the call to [xde_derivatives] dispatches on `xds_obj$frame`
#' @description Implements [xds_solve] for ordinary differential equations
#' @inheritParams xds_solve
#' @return an **`xds`** object
#'
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
#' @description Implements [xds_solve] for delay differential equations
#' @inheritParams xds_solve
#' @return an **`xds`** object
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
#'
#' @note For discrete time systems, the values in `times` need not be 
#' a sequence. If `times` is not null, the system iterates up to `max(times)` 
#' and then returns the solutions at the subset specified in `times.` 
#' 
#'  
#' @inheritParams xds_solve
#' @return a [list]
#' @export
xds_solve.dts = function(xds_obj, Tmax=365, dt=1, times=NULL){
  
  xds_obj <- check_models(xds_obj)
  tm <- make_times_dts(Tmax, dt, times)
  yt = get_inits(xds_obj)
  
  dts_out = c(0, yt)
  for(t in tm[-1]){
    yt =  dts_update(t, yt, xds_obj)
    dts_out = rbind(dts_out, c(t,yt))
  }
  
  xds_obj$outputs$deout <- dts_out
  ix = which(times %in% tm)
  xds_obj <- parse_outputs(xds_obj, dts_out[ix,], tm)
  return(xds_obj)
}


#' @title Times Utility for Differential Equations
#' 
#' @description This method sets up the time points for the
#' independent variable to return solutions for the dependent variables. 
#' 
#' If `times` is not `NULL,` then it returns solutions at those values.
#' 
#' Otherwise, it returns a sequence from `0` to `Tmax` by `dt.`
#' 
#' If values in `times` are not a sequence, the function checks that 
#' all its values are in the sequence of time points when solutions are returned. 
#' 
#' @param Tmax the last time point, run from 0...Tmax
#' @param dt the time interval for outputs
#' @param times the times
#' 
#' @return an **`xds`** object
#' @export
make_times_xde = function(Tmax, dt, times=NULL){
  if(!is.null(times)) return(times)
  return(seq(0, Tmax, by=dt))
}

#' @title Times Utility for Discrete Time Systems 
#' 
#' @description This method sets up the time points when output is wanted. 
#' For discrete time systems, the system must compute values at every point,
#' but it is possible  
#' 
#' If `times` is not null, it sets `maxT` to the maximum value in times. 
#' Otherwise it sets `maxT=Tmax.` 
#'
#' It returns a sequence from `0` to `maxT` by `dt.`
#' 
#' A check 
#' values of `times` should be some integer multiple of `dt`
#' 
#' @param Tmax the last time point, run from 0...Tmax
#' @param dt the time interval for outputs
#' @param times the times
#' 
#' @return an **`xds`** object
#' @export
make_times_dts = function(Tmax=365, dt=1, times=NULL){
  maxT = ifelse(!is.null(times), max(times), Tmax)
  tm = seq(0, maxT, by=dt)
  ll <- which(times %in% tm)
  stopifnot(length(ll) == length(times))
  return(tm)
}
