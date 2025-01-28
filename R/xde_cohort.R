
#' @title Reset the mean daily EIR
#' @description For cohort models, reset the EIR
#' @param eir the mean eir
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
set_eir  = function(eir, pars){
  UseMethod("set_eir", pars$frame)
}

#' @title Reset the mean daily EIR
#' @description For cohort models, reset the EIR
#' @param eir the mean eir
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
set_eir.cohort  = function(eir, pars){

  F_eir <- with(pars$EIRpar, function(age, bday){
    F_season(age+bday)
  })

  stats::integrate(F_eir, 0, 365, bday=0)$val -> scale

  pars$EIRpar$scale = scale/365
  pars$EIRpar$eir = eir

  with(pars$EIRpar,{
    pars$F_eir <- function(age, bday){
      eir/scale*F_season(age+bday)*F_trend(age+bday)*F_age(age)}
    return(pars)
  })
}

#' @title Cohort dynamics for a human / host model
#' @description
#' Compute the states for a model \eqn{\cal X} in a cohort of humans / hosts
#' as it ages, up to age \eqn{A} years of age
#' @details
#' This method substitutes age for time: a model
#' \deqn{\cal X(t)}
#' is solved with respect to age \eqn{a}:
#' \deqn{\cal X(a),}
#' where the daily EIR is computed by a *trace* function with four elements:
#' + \eqn{\bar E} or `eir`, the mean daily EIR,
#' + \eqn{\omega(a)} or `F_age,` a function of age
#' + \eqn{S(t)} or `F_season,` a function of time of year
#' + \eqn{T(t)} or `F_trend,` a function describing a trend
#'
#' For a cohort born on day \eqn{B},
#' the function creates a mesh on age / time, where time and age
#' are related by the formula:
#' \deqn{t = B + a}
#' and the trace function is:
#'  \deqn{E(a, t) = \hat E \; \omega(a) \; S(t)\; T(t) }
#' The output is returned as `pars$outputs$cohort`
#' @param pars an **`xds`** object
#' @param bday the cohort birthday
#' @param A the maximum age to compute (in years)
#' @param da the output interval (age, in days)
#' @param times the output times 
#' @return an **`xds`** object
#' @export
xds_solve_cohort = function(pars, bday=0, A=10, da=10, times=NULL){

  pars <- set_eir(pars$EIRpar$eir, pars)

  age <- seq(0, A*365, by=da) 
  if(!is.null(times)){
    age <- times
    bday <- 0
  } 
  
  y0 = get_inits(pars, flatten=TRUE)

  xde_cohort_desolve(bday, y0, age, pars) -> deout
  de_vars <- deout[,-1]

  pars$outputs$orbits <- list()
  pars$outputs$orbits$XH <- list()
  pars$outputs$orbits$XH[[1]] <- parse_orbits(de_vars, pars)$XH[[1]]
  pars$outputs$last_y <- tail(de_vars, 1)
  pars$outputs$orbits$age <- age
  pars$outputs$orbits$time <- age+bday
  pars$outputs$time <- age+bday
  pars$outputs$terms <- list()
  pars$outputs$terms$EIR <- list()
  pars$outputs$terms$EIR[[1]] <- with(pars, F_eir(age, bday))
  return(pars)
}

#' @title Differential equation models for human cohorts
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @param age host age
#' @param y the state variables
#' @param pars an **`xds`** object
#' @param birthday the cohort birthday
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_cohort_derivatives <- function(age, y, pars, birthday) {

  t = age+birthday

  # EIR: entomological inoculation rate trace
  pars$EIR[[1]] <- pars$F_eir(age, birthday)

  # FoI: force of infection
  pars <- Exposure(t, y, pars)

  # state derivatives
  dX <- dXdt(age, y, pars, 1)

  return(list(c(dX)))
}

#' @title Solve a system of equations as an ode
#' @description Implements for ordinary differential equations
#' @param birthday a cohort birthday
#' @param inits initial values
#' @param times = the times
#' @param pars an **`xds`** object
#' @return a [list]
#' @export
xde_cohort_desolve  = function(birthday, inits, times, pars){
  UseMethod("xde_cohort_desolve", pars$xds)
}

#' @title Solve a system of equations as a dde
#' @description Implements for delay differential equations
#' @inheritParams xde_cohort_desolve
#'@return a [list]
#' @export
xde_cohort_desolve.dde = function(birthday, inits, times, pars){
  return(deSolve::dede(y=inits, times=times, func=xde_cohort_derivatives, parms=pars,
                       method = "lsoda", birthday=birthday))
}

#' @title Solve a system of equations as a ode
#' @description Implements for delay differential equations
#' @inheritParams xde_cohort_desolve
#'@return a [list]
#' @export
xde_cohort_desolve.ode = function(birthday, inits, times, pars){
  return(deSolve::ode(y=inits, times=times, func=xde_cohort_derivatives, parms = pars,
                      method = "lsoda", birthday=birthday))
}

