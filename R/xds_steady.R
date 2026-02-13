
#' @title Compute stable orbits 
#' 
#' @description 
#' Run the system for 10 years, and save the last year's orbits.
#'  
#' @param xds_obj an **`xds`** model object
#' @param options a list of configurable options 
#' @return an **`xds`** model object
#' @export
xds_stable_orbit = function(xds_obj, options=list()){
  UseMethod("xds_stable_orbit", xds_obj$xds)
}

#' @title Compute stable orbit for a system of differential equations
#' @description This method dispatches on the type of `xds_obj$xde`.
#' @inheritParams xds_stable_orbit
#' @return an **`xds`** object
#' @noRd
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
#' @description This method dispatches on the type of `xds_obj$xde`.
#' @inheritParams xds_stable_orbit
#' @return an **`xds`** object
#' @noRd
#' @export
xds_stable_orbit.dts = function(xds_obj, options=list()){
  stop(error="Not Written Yet")   
  return(xds_obj)
}


#' @title Solve for the steady state of a system of equations using [rootSolve::steady]
#' @description This method dispatches on the type of `xds_obj$xde`
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @export
xds_steady = function(xds_obj){
  UseMethod("xds_steady", xds_obj$xde)
}

#' @title Solve for the steady state of a system of equations using [rootSolve::steady]
#' @note This method dispatches on `class(dlay)`
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @noRd
#' @export
xds_steady.ode = function(xds_obj){
  y0 = get_inits(xds_obj, flatten=TRUE)
  rootSolve::steady(y=y0, func = xde_derivatives, parms = xds_obj)$y -> y_eq
  xds_obj$outputs$steady = parse_y(y_eq, xds_obj)
  return(xds_obj)
}

#' @title Solve for the steady state of a system of equations using [rootSolve::steady]
#' @description This method dispatches on the type of `xds_obj$xde`
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @noRd
#' @export
xds_steady.dde = function(xds_obj){
  y0 = get_inits(xds_obj, flatten=TRUE)
  rootSolve::runsteady(y=y0, func = xde_derivatives, parms = xds_obj)$y -> y_eq
  xds_obj$outputs$steady = parse_y(y_eq, xds_obj)
  return(xds_obj)
}



#' @title Solve for the steady state or stable orbit of a system of equations
#' @description This method dispatches on the type of `xds_obj$dts`.
#' @param xds_obj an **`xds`** model object
#' @param Tburn the number of steps to burn
#' @param yr the number of time steps in a year
#' @return a [list]
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

#' @title Solve for the steady state of a system of equations
#' @description This method dispatches on the type of `xds_obj$dts`
#' @param xds_obj an **`xds`** model object
#' @param Tx the number of steps to equilibrium
#' @return a [list]
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

