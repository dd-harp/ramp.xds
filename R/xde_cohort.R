

#' @title Cohort orbits for a model \eqn{\cal X}
#' @description
#' Compute the states for a model \eqn{\cal X} in a cohort of humans / hosts
#' as it ages, up to age \eqn{A} years of age
#' @details
#' The treats treats the model
#' \deqn{\cal X(t)}
#' as if it were a function of age \eqn{a}:
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
#' @param eir the population average daily eir
#' @param bday a cohort's birthday
#' @param pars a [list] that defines a model
#' @param A the maximum age (in years)
#' @param da the output interval (age, in days)
#' @param F_season a function describing a seasonal pattern over time
#' @param F_trend a function describing a temporal trend over time
#' @param F_age a function assigning a biting weight by age
#' @return an **`xds`** object
#' @export
xde_solve_cohort = function(eir, bday, pars, A=10, da=10,
                      F_season = NULL, F_trend=NULL, F_age=NULL){

  if(!is.null(F_season)) pars$EIRpar$F_season = F_season
  if(!is.null(F_trend)) pars$EIRpar$F_trend = F_trend
  if(!is.null(F_age)) pars$EIRpar$F_age = F_age

  F_eir <- with(pars$EIRpar, function(age, bday){
    eir*F_season(age+bday)*F_trend(age+bday)*F_age(age)
  })
  pars$F_eir = F_eir

  age = seq(0, A*365, by=da)
  y0 = get_inits(pars, flatten=TRUE)

  xde_cohort_desolve(bday, y0, age, pars) -> deout
  de_vars <- deout[,-1]
  pars$outputs$cohort <- parse_orbits(de_vars, pars)
  pars$outputs$cohort$age <- age
  pars$outputs$cohort$time <- age+bday
  pars$outputs$cohort$eir <- F_eir(age, bday)

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
  UseMethod("xde_cohort_desolve", pars$dlay)
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

