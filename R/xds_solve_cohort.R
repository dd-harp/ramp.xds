#' @title Solve Cohort Dynamics
#'
#' @description
#' 
#' Given a trace function describing the average EIR in a population over time, 
#' simulate malaria in a cohort as it ages: the independent variable is age 
#' \eqn{(a)} and not time \eqn{(t)}.  
#' 
#' Relative biting rates by 
#' age are set by \eqn{F_\omega(a)} or `F_age` with associated parameters `age_par`. 
#' 
#' To solve, the argument `birthday` sets the birthday for a cohort, \eqn{(d)}.
#' The user ether passes `ages` or `Amax` and `da` to configure a vector of 
#' ages at which output is wanted \eqn{(a_i)}, at times \eqn{t_i=a_i+d}.  
#' 
#' In effect, since \eqn{a = t-d}, the function solves the  
#' solves the system for for an exposure function with the pattern:
#' \deqn{F_w(a) \times F_S(t-d) \times F_T(t-d) \times F_K(t-d)}
#'
#' @note
#' Use `xds_setup_eir` to set up a model for cohort dynamics. 
#' 
#' During setup, the variable `xds_obj$EIR_obj$bday` is set to 0, 
#' and `F_age` is set to `F_one`. 
#'
#' @param xds_obj an **`xds`** model object
#' @param birthday a cohort's birthday
#' @param Amax the oldest year, run from 0...Amax
#' @param da the age interval
#' @param ages a set of ages
#'
#' @export
xds_solve_cohort = function(xds_obj, birthday=0, Amax=365, da=1, ages=NULL){
  stopifnot(class(xds_obj) == "eir")
  xds_obj$EIR_obj$bday = birthday
  xds_obj <- xds_solve(xds_obj, Tmax=birthday+Amax, dt=da, times=ages+birthday)
  return(xds_obj)
}