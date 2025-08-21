
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
#' The output is returned as `xds_obj$outputs$cohort`
#' @param xds_obj a **`ramp.xds`** model object
#' @param bday the cohort birthday
#' @param A the maximum age to compute (in years)
#' @param da the output interval (age, in days)
#' @return an **`xds`** object
#' @export
xds_solve_cohort = function(xds_obj, bday=0, A=10, da=10){

  age <- seq(0, A*365, by=da) 
  
  y0 = get_inits(xds_obj, flatten=TRUE)

  xde_cohort_desolve(bday, y0, age, xds_obj) -> deout
  de_vars <- deout[,-1]

  xds_obj$outputs$orbits <- list()
  xds_obj$outputs$orbits$XH <- list()
  xds_obj$outputs$orbits$XH[[1]] <- parse_orbits(de_vars, xds_obj)$XH[[1]]
  xds_obj$outputs$last_y <- tail(de_vars, 1)
  xds_obj$outputs$orbits$age <- age
  xds_obj$outputs$orbits$time <- age+bday
  tm <- age + bday
  xds_obj$outputs$time <- tm 
  xds_obj$outputs$terms <- list()
  xds_obj$outputs$terms$EIR <- list()
  xds_obj$outputs$terms$EIR[[1]] <- with(xds_obj$EIRpar, eir*F_season(tm)*F_trend(tm)*F_age(age))
  return(xds_obj)
}

#' @title Differential equation models for human cohorts
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @param age host age
#' @param y the state variables
#' @param xds_obj a **`ramp.xds`** model object
#' @param birthday the cohort birthday
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_cohort_derivatives <- function(age, y, xds_obj, birthday) {

  t = age+birthday

  # EIR: entomological inoculation rate trace
  xds_obj$EIR[[1]] <- with(xds_obj$EIRpar, eir*F_trend(t)*F_season(t)*F_age(age))

  # FoI: force of infection
  xds_obj <- Exposure(t, y, xds_obj)

  # state derivatives
  dX <- dXdt(age, y, xds_obj, 1)

  return(list(c(dX)))
}

#' @title Solve a system of equations as an ode
#' @description Implements for ordinary differential equations
#' @param birthday a cohort birthday
#' @param inits initial values
#' @param times = the times
#' @param xds_obj a **`ramp.xds`** model object
#' 
#' @return a [list]
#' @export
xde_cohort_desolve  = function(birthday, inits, times, xds_obj){
  UseMethod("xde_cohort_desolve", xds_obj$xde)
}

#' @title Solve a system of equations as a dde
#' @description Implements for delay differential equations
#' @inheritParams xde_cohort_desolve
#'@return a [list]
#' @export
xde_cohort_desolve.dde = function(birthday, inits, times, xds_obj){
  return(deSolve::dede(y=inits, times=times, func=xde_cohort_derivatives, parms=xds_obj,
                       method = "lsoda", birthday=birthday))
}

#' @title Solve a system of equations as a ode
#' @description Implements for delay differential equations
#' @inheritParams xde_cohort_desolve
#'@return a [list]
#' @export
xde_cohort_desolve.ode = function(birthday, inits, times, xds_obj){
  return(deSolve::ode(y=inits, times=times, func=xde_cohort_derivatives, parms = xds_obj,
                      method = "lsoda", birthday=birthday))
}

