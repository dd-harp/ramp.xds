
#' @title Solve a discrete-time system
#' @description Implements [dts_solve] for equations
#' @param pars a [list] that defines a model
#' @param ts the number of time steps
#' @return a [list]
#' @export
dts_solve = function(pars, ts=365){
  tt = seq(0, ts, by=pars$Dday)
  yt = get_inits(pars)
  dts_out = c(0, yt)
  for(t in tt[-1]){
    yt =  dts_update(t, yt, pars)
    dts_out = rbind(dts_out, c(t,yt))
  }
  pars <- make_outputs(pars, dts_out, tt[-1])
  pars$outputs$deout <- dts_out
  return(pars)
}

#' @title Solve for the steady state or stable orbit of a system of equations
#' @description This method dispatches on the type of `pars$dts`.
#' @param pars a [list] that defines a model
#' @param Tburn the number of steps to burn
#' @param yr the number of time steps in a year
#' @return a [list]
#' @export
dts_stable_orbit = function(pars, Tburn=10, yr=365){
  yt = get_inits(pars)
  tt = seq(0, Tburn*yr, by=pars$Dday)
  for(t in tt){
    yt =  dts_update(t, yt, pars)
  }
  orbit = yt
  tt = seq(0, yr, by=pars$Dday)
  for(t in tt[-1]){
    yt =  dts_update(t, yt, pars)
    orbit = rbind(orbit, yt)
  }
  pars <- make_outputs(pars, orbit, tt[-1])
  pars$outputs$deout <- orbit
  return(pars)
}

#' @title Solve for the steady state of a system of equations
#' @description This method dispatches on the type of `pars$dts`
#' @param pars a [list] that defines a model
#' @param Tx the number of steps to equilibrium
#' @return a [list]
#' @export
dts_steady = function(pars, Tx=1000){
  tt = seq(0, Tx, by=pars$Dday)
  yt = get_inits(pars)
  for(t in tt[-1]){
    yt =  dts_update(t, yt, pars)
  }
  pars$outputs$steady = parse_y(yt, pars)
  return(pars)
}


