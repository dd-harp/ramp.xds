# X

#' @title Solve Cohort Dynamics
#'
#' @description
#' 
#' Given a forcing function describing the average EIR in a population over time,
#' simulate malaria in a cohort as it ages, using age as the independent variable
#' \eqn{(a)} rather than time \eqn{(t)}.
#' 
#' Relative biting rates by 
#' age are set by \eqn{F_\omega(a)} or `F_age` with associated parameters `age_par`. 
#' 
#' To solve, the argument `birthday` sets the birthday for a cohort, \eqn{(d)}.
#' The user either passes `ages` or `Amax` and `da` to configure a vector of
#' ages at which output is wanted \eqn{(a_i)}, at times \eqn{t_i=a_i+d}.  
#' 
#' In effect, since \eqn{a = t-d}, the function solves the system for an
#' exposure function with the pattern:
#' \deqn{F_\omega(a) \times F_S(t-d) \times F_T(t-d) \times F_K(t-d)}
#'
#' @note
#' Use `xds_setup_eir` to set up a model for cohort dynamics. 
#' 
#' During setup, the variable `xds_obj$EIR_obj$bday` is set to 0, 
#' and `F_age` is set to `F_one`. 
#'
#' For discrete-time models, calendar output times (birthday + ages) must be whole-number time steps.
#'
#' @param xds_obj an **`xds`** model object
#' @param birthday a cohort's birthday
#' @param eir the mean eir
#' @param Amax the oldest age, run from 0...Amax
#' @param da the age interval
#' @param ages a set of ages
#'
#' @export
xds_solve_cohort = function(xds_obj, birthday=0, eir = c(), Amax=365, da=1, ages=NULL){
  stopifnot(class(xds_obj$frame) == "eir")
  if (inherits(xds_obj$xds, "dts")) birthday <- round(birthday)
  if(length(eir)>0) xds_obj$EIR_obj$eir = eir
  xds_obj$EIR_obj$bday = birthday
  times <- make_ages(xds_obj, Amax, da, ages) + birthday
  
  xds_obj <- xds_solve(xds_obj, times=times)
  return(xds_obj)
}

#' @title Make cohort ages
#' @inheritParams xds_solve_cohort
#' @keywords internal
make_ages <- function(xds_obj, Amax=365, da=1, ages=NULL){
  UseMethod("make_ages", xds_obj$xds)
}
#' @title Make cohort ages

#' @inheritParams make_ages
#' @keywords internal
#' @export
make_ages.xde <- function(xds_obj, Amax=365, da=1, ages=NULL){
  stopifnot(da>0)
  make_times_xde(Amax, da, ages)
}
#' @title Make cohort ages

#' @inheritParams make_ages
#' @keywords internal
#' @export
make_ages.dts <- function(xds_obj, Amax=365, da=1, ages=NULL){
  stopifnot(da>0)
  make_times_dts(Amax, da, ages) 
}
