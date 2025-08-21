
#' @title Compute the stable orbit for a system of differential equations
#' @description This method dispatches on the type of `pars$xde`.
#' @param pars an **`xds`** object
#' @param Ymax the number of years to burn-in
#' @return an **`xds`** object
#' @export
xde_stable_orbit = function(pars, Ymax=10){
  pars <- xds_solve(pars, Tmax = Ymax*365, dt=1)
  deout = tail(pars$outputs$orbits$deout, 365)
  tm = deout[,1]
  pars <- make_outputs(pars, deout[,-1], tm)
  pars$outputs$tm <- tm %% 365
  pars$outputs$deout <- deout
  return(pars)
}

#' @title Solve for the steady state of a system of equations using [rootSolve::steady]
#' @description This method dispatches on the type of `pars$xde`
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
xde_steady = function(pars){
  UseMethod("xde_steady", pars$dlay)
}

#' @title Solve for the steady state of a system of equations using [rootSolve::steady]
#' @note This method dispatches on `class(dlay)`
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
xde_steady.ode = function(pars){
  y0 = get_inits(pars, flatten=TRUE)
  rootSolve::steady(y=y0, func = xde_derivatives, parms = pars)$y -> y_eq
  pars$outputs$steady = parse_y(y_eq, pars)
  return(pars)
}

#' @title Solve for the steady state of a system of equations using [rootSolve::steady]
#' @description This method dispatches on the type of `pars$xde`
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
xde_steady.dde = function(pars){
  y0 = get_inits(pars, flatten=TRUE)
  rootSolve::runsteady(y=y0, func = xde_derivatives, parms = pars)$y -> y_eq
  pars$outputs$steady = parse_y(y_eq, pars)
  return(pars)
}

