#' @title Solve a system of differential equations
#' @description This method solves a system of equations and returns the values of
#' the dependent variables at times \eqn{t=0, dt, 2dt, ... Tmax}. The function dispatches
#' on `pars$dlay` to call:
#'
#'   - [deSolve::ode] if `class(dlay) == 'ode'`
#'   - [deSolve::dede] if `class(dlay) == 'dde'`
#'
#' Note that the call to [xde_derivatives] dispatches on `pars$frame`
#'
#' @seealso [xde_derivatives()]
#' @param pars a [list] that defines a model
#' @param Tmax the last time point, run from 0...Tmax
#' @param dt the time interval for outputs
#' @return a [list]
#' @export
xde_solve = function(pars, Tmax=365, dt=1){
  UseMethod("xde_solve", pars$dlay)
}

#' @title Solve a system of equations using [deSolve::ode]
#' @description This method solves a system of equations and returns the values of
#' the dependent variables at times \eqn{t=0, dt, 2dt, ... Tmax}. The function dispatches
#' on `pars$dlay` to call:
#'
#'   - [deSolve::ode] if `class(dlay) == 'ode'`
#'   - [deSolve::dede] if `class(dlay) == 'dde'`
#'
#' Note that the call to [xde_derivatives] dispatches on `pars$frame`
#' @description Implements [xde_solve] for ordinary differential equations
#' @inheritParams xde_solve
#' @return a [list]
#' @export
xde_solve.ode = function(pars, Tmax=365, dt=1){
  tt = seq(0, Tmax, by=dt)
  y0 = get_inits(pars, flatten=TRUE)
  deSolve::ode(y = y0, times = tt, func = xde_derivatives, parms = pars, method = "lsoda") -> deout
  tm <- deout[,1]
  pars <- make_outputs(pars, deout[,-1], tm)
  pars$outputs$deout <- deout
  return(pars)
}

#' @title Solve a system of equations using [deSolve:dde]
#' @description Implements [xde_solve] for delay differential equations
#' @inheritParams xde_solve
#'@return a [list]
#' @export
xde_solve.dde = function(pars, Tmax=365, dt=1){
  tt = seq(0, Tmax, by=dt)
  y0 = get_inits(pars, flatten=TRUE)
  deSolve::dede(y = y0, times = tt, func = xde_derivatives, parms = pars, method = "lsoda") -> deout
  tm <- deout[,1]
  pars <- make_outputs(pars, deout[,-1], tm)
  pars$outputs$deout <- deout
  return(pars)
}

#' @title Solve for the steady state or stable orbit of a system of equations
#' @description This method dispatches on the type of `pars$xde`.
#' @param pars a [list] that defines a model
#' @param Ymax the number of years to burn-in
#' @return a [list]
#' @export
xde_stable_orbit = function(pars, Ymax=10){
  pars <- xde_solve(pars, Tmax = Ymax*365, dt=1)
  deout = tail(pars$outputs$orbits$deout, 365)
  tm = deout[,1]
  pars <- make_outputs(pars, deout[,-1], tm)
  pars$outputs$tm <- tm %% 365
  pars$outputs$deout <- deout
  return(pars)
}

#' @title Solve for the steady state of a system of equations using [rootSolve::steady]
#' @description This method dispatches on the type of `pars$xde`
#' @param pars a [list] that defines a model
#' @return a [list]
#' @export
xde_steady = function(pars){
  UseMethod("xde_steady", pars$dlay)
}

#' @title Solve for the steady state of a system of equations using [rootSolve::steady]
#' @description This method dispatches on the type of `pars$xde`
#' @param pars a [list] that defines a model
#' @return a [list]
#' @export
xde_steady.ode = function(pars){
  y0 = get_inits(pars, flatten=TRUE)
  rootSolve::steady(y=y0, func = xde_derivatives, parms = pars)$y -> y_eq
  pars$outputs$steady = parse_y(y_eq, pars)
  return(pars)
}

#' @title Solve for the steady state of a system of equations using [rootSolve::steady]
#' @description This method dispatches on the type of `pars$xde`
#' @param pars a [list] that defines a model
#' @return a [list]
#' @export
xde_steady.dde = function(pars){
  y0 = get_inits(pars, flatten=TRUE)
  rootSolve::runsteady(y=y0, func = xde_derivatives, parms = pars)$y -> y_eq
  pars$outputs$steady = parse_y(y_eq, pars)
  return(pars)
}

