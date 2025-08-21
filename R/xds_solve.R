
#' @title Solve a system of differential equations
#' @description This method solves a system of equations and returns the values of
#' the dependent variables at `times` or at  \eqn{t=0, dt, 2\;dt, ... Tmax}.
#'
#' @note Note that [xde_derivatives] dispatches on `class(frame)`
#'
#' @seealso [xde_derivatives()]
#' @param pars a [list] that defines a model
#' @param Tmax the last time point, run from 0...Tmax
#' @param dt the time interval for outputs
#' @param times the times
#' @return pars an **`xds`** object
#' @export
xds_solve = function(pars, Tmax=365, dt=1, times=NULL){
  UseMethod("xds_solve", pars$xde)
}

#' @title Solve a system of equations using [deSolve::ode]
#' @description This method solves a system of equations and computes the values of
#' the dependent variables at times \eqn{t=0, dt, 2dt, ... Tmax}. The function dispatches
#' on `pars$dlay` to call:
#'
#'   - [deSolve::ode] if `class(dlay) == 'ode'`
#'   - [deSolve::dede] if `class(dlay) == 'dde'`
#'
#' Note that the call to [xde_derivatives] dispatches on `pars$frame`
#' @description Implements [xds_solve] for ordinary differential equations
#' @inheritParams xds_solve
#' @return an **`xds`** object
#'
#' @export
xds_solve.ode = function(pars, Tmax=365, dt=1, times=NULL){
  tt <- seq(0, Tmax, by=dt)
  if(!is.null(times)) tt <- times
  y0 = get_inits(pars, flatten=TRUE)
  deSolve::ode(y = y0, times = tt, func = xde_derivatives, parms = pars, method = "lsoda") -> deout
  deout <- matrix(deout, nrow = length(tt), ncol = length(y0)+1)
  tm <- as.vector(deout[,1])
  pars <- make_outputs(pars, as.matrix(deout[,-1]), tm)
  pars$outputs$deout <- deout
  return(pars)
}

#' @title Solve a system of equations using [deSolve::dede]
#' @description Implements [xds_solve] for delay differential equations
#' @inheritParams xds_solve
#' @return an **`xds`** object
#' @export
xds_solve.dde = function(pars, Tmax=365, dt=1, times=NULL){
  tt <- seq(0, Tmax, by=dt)
  if(!is.null(times)) tt <- times
  y0 = get_inits(pars, flatten=TRUE)
  deSolve::dede(y = y0, times = tt, func = xde_derivatives, parms = pars, method = "lsoda") -> deout
  deout <- matrix(deout, nrow = length(tt), ncol = length(y0)+1)
  tm <- as.vector(deout[,1])
  pars <- make_outputs(pars, as.matrix(deout[,-1]), tm)
  pars$outputs$deout <- deout
  return(pars)
}

#' @title Solve a discrete-time system
#' @description Implements [xds_solve] for equations
#' @inheritParams xds_solve
#' @return a [list]
#' @export
xds_solve.dts = function(pars, Tmax=365, dt=NULL, times=NULL){
  tt <- seq(0, Tmax, by=1)
  yt = get_inits(pars)
  dts_out = c(0, yt)
  for(t in tt[-1]){
    yt =  dts_update(t, yt, pars)
    dts_out = rbind(dts_out, c(t,yt))
  }
  if(!is.null(times)){
    ix = which(times %in% tt)
    dts_out <- dts_out[ix,]
  }
  pars <- make_outputs(pars, dts_out, times)
  pars$outputs$deout <- dts_out
  return(pars)
}
