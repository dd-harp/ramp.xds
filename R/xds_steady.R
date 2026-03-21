
#' @title Compute stable orbits
#'
#' @description
#' Run the system for 10 years, and save the last year's orbits.
#'
#' @param xds_obj an **`xds`** model object
#' @param options a list of configurable options
#' @return an **`xds`** object
#' @export
xds_stable_orbit = function(xds_obj, options=list()){
  UseMethod("xds_stable_orbit", xds_obj$xds)
}

#' @title Compute stable orbit for a system of differential equations
#' @description 
#' Run a system of differential equations for 10 years, and save the last year's orbits.
#' @inheritParams xds_stable_orbit
#' @return an **`xds`** object
#' @keywords internal
#' @export
xds_stable_orbit.xde = function(xds_obj, options=list()){
  Tmax = 3650
  with(options,{
    xds_obj <- xds_solve(xds_obj, Tmax = Tmax, dt=1)
    deout = tail(xds_obj$outputs$orbits$deout, 365)
    tm = deout[,1]
    xds_obj <- parse_outputs(xds_obj, deout[,-1], tm)
    xds_obj$outputs$tm <- tm %% 365
    xds_obj$outputs$deout <- deout
  })
  return(xds_obj)
}

#' @title Compute the stable orbit for a discrete time system
#' @description 
#' Run a discrete time system for 10 years, and save the last year's orbits.
#' @inheritParams xds_stable_orbit
#' @return an **`xds`** object
#' @keywords internal
#' @export
xds_stable_orbit.dts = function(xds_obj, options=list()){
  stop(error="Not Written Yet")
  return(xds_obj)
}


#' @title Solve for the steady state 
#' @description Compute the steady state for a 
#' system of differential equations.  
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @export
xds_steady = function(xds_obj){
  UseMethod("xds_steady", xds_obj$xde)
}

#' @title Compute the steady state for ODEs
#' @description Call `rootSolve` to get the steady state of a
#' system of ordinary differential equations.
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @keywords internal
#' @export
xds_steady.ode = function(xds_obj){
  y0 = get_inits(xds_obj, flatten=TRUE)
  rootSolve::steady(y=y0, func = xde_derivatives, parms = xds_obj)$y -> y_eq
  xds_obj$outputs$steady = parse_y(y_eq, xds_obj)
  return(xds_obj)
}

#' @title Compute the steady state for DDEs
#' @description Call `runsteady` to get the steady state of a
#' system of delay differential equations.
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @keywords internal
#' @export
xds_steady.dde = function(xds_obj){
  y0 = get_inits(xds_obj, flatten=TRUE)
  rootSolve::runsteady(y=y0, func = xde_derivatives, parms = xds_obj)$y -> y_eq
  xds_obj$outputs$steady = parse_y(y_eq, xds_obj)
  return(xds_obj)
}

#' @title Compute the stable orbit for a discrete time system
#' @description Run a discrete time system for `Tburn` years
#' to get the stable orbit
#' @param xds_obj an **`xds`** model object
#' @param Tburn the number of steps to burn
#' @param yr the number of time steps in a year
#' @return an **`xds`** object
#' @export
dts_stable_orbit = function(xds_obj, Tburn=10, yr=365){
  yt = get_inits(xds_obj)
  tt = seq(0, Tburn*yr, by=xds_obj$Dday)
  for(t in tt){
    yt =  dts_update(t, yt, xds_obj)
  }
  orbit = yt
  tt = seq(0, yr, by=xds_obj$Dday)
  for(t in tt[-1]){
    yt =  dts_update(t, yt, xds_obj)
    orbit = rbind(orbit, yt)
  }
  xds_obj <- parse_outputs(xds_obj, orbit, tt[-1])
  xds_obj$outputs$deout <- orbit
  return(xds_obj)
}

#' @title Compute the steady state of a discrete time system
#' @description Run a discrete time system for `Tx` time steps to get the steady state
#' @param xds_obj an **`xds`** model object
#' @param Tx the number of steps to equilibrium
#' @return an **`xds`** object
#' @export
dts_steady = function(xds_obj, Tx=1000){
  tt = seq(0, Tx, by=xds_obj$Dday)
  yt = get_inits(xds_obj)
  for(t in tt[-1]){
    yt =  dts_update(t, yt, xds_obj)
  }
  xds_obj$outputs$steady = parse_y(yt, xds_obj)
  return(xds_obj)
}

