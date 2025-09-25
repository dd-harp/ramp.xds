
#' @title Compute Derivatives
#' 
#' @description The function to compute the derivatives dispatches on `class(frame)`
#'
#' @seealso [xds_solve]
#' 
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' 
#' @return a [list] containing the vector of all state derivatives
#' 
#' @export
xds_compute_terms <- function(t, y, xds_obj) {
  UseMethod("xds_compute_terms", xds_obj$frame)
}

#' @title Generalized spatial differential equation model
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' 
#' @inheritParams xds_compute_terms
#' 
#' @return a [list] containing the vector of all state derivatives
#' @export
xds_compute_terms.full <- function(t, y, xds_obj) {
  # set the values of Exogenous forcing variables
  xds_obj <- Forcing(t, xds_obj)
  # implement malaria control
  xds_obj <- Health(t, y, xds_obj)
  # implement vector control 
  xds_obj <- VectorControl(t, y, xds_obj)
  # set the values of resource variables -- 
  xds_obj <- Resources(t, y, xds_obj)
  # blood feeding: available blood hosts, TaR, relative biting rates
  xds_obj <- BloodFeeding(t, y, xds_obj)
  # egg laying: available habitat, egg distribution matrix
  xds_obj <- EggLaying(t, y, xds_obj)
  # Compute baseline mosquito bionomic parameters
  # (including available sugar)
  xds_obj <- BaselineBionomics(t, y, xds_obj)
  # Compute  "independent effect sizes"
  xds_obj <- VectorControlEffectSizes(t, y, xds_obj)
  # Modify the baseline
  xds_obj <- ModifiedBionomics(t, y, xds_obj)
  # emergence: Lambda
  xds_obj <- Emergence(t, y, xds_obj)
  # transmission: beta, EIR, and kappa
  xds_obj <- Transmission(t, y, xds_obj)
  # compute the FoI
  xds_obj <- Exposure(t, y, xds_obj)
  return(xds_obj)
}

#' @title Differential equations isolating the humans, forced with Ztrace
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @inheritParams xds_compute_terms
#' @return a [list] containing the vector of all state derivatives
#' @export
xds_compute_terms.human <- function(t, y, xds_obj) {
  # weather port -- see ramp.forcing
  xds_obj <- Forcing(t, xds_obj)
  # malaria control port -- see ramp.control
  xds_obj <- Health(t, y, xds_obj)
  # vector control port -- see ramp.control
  xds_obj <- VectorControl(t, y, xds_obj)
  # set the values of resource variables
  xds_obj <- Resources(t, y, xds_obj)
  # blood feeding: available blood hosts, TaR, relative biting rates
  xds_obj <- BloodFeeding(t, y, xds_obj)
  # Compute baseline mosquito bionomic parameters
  # (including available sugar)
  xds_obj <- BaselineBionomics(t, y, xds_obj)
  # Compute total effect size assuming
  # "independent effect sizes"
  xds_obj <- VectorControlEffectSizes(t, y, xds_obj)
  # Modify the baseline
  xds_obj <- ModifiedBionomics(t, y, xds_obj)
  # compute beta, EIR, and kappa
  xds_obj <- Transmission(t, y, xds_obj)
  # compute the FoI
  xds_obj <- Exposure(t, y, xds_obj)
  return(xds_obj)
}

#' @title Generalized spatial differential equation model (mosquito only)
#' @description Mirrors [ramp.xds::xds_compute_terms] but only includes the adult and aquatic
#' mosquito components.
#' @inheritParams xds_compute_terms
#' @return a [list] containing the vector of all state derivatives
#' @export
xds_compute_terms.mosy <- function(t, y, xds_obj){
  
  # set the values of Exogenous forcing variables
  xds_obj <- Forcing(t, xds_obj)
  # vector control
  xds_obj <- VectorControl(t, y, xds_obj)
  # set the values of resource variables
  xds_obj <- Resources(t, y, xds_obj)
  # blood feeding: available blood hosts, TaR, relative biting rates
  xds_obj <- BloodFeeding(t, y, xds_obj)
  # egg laying: available habitat, egg distribution matrix
  xds_obj <- EggLaying(t, y, xds_obj)
  # Compute baseline mosquito bionomic parameters
  # (including available sugar)
  xds_obj <- BaselineBionomics(t, y, xds_obj)
  # Compute total effect size assuming
  # "independent effect sizes"
  xds_obj <- VectorControlEffectSizes(t, y, xds_obj)
  # Modify the baseline
  xds_obj <- ModifiedBionomics(t, y, xds_obj)
  # emergence: compute Lambda
  xds_obj <- Emergence(t, y, xds_obj)
  return(xds_obj)  
}

#' @title Differential equation models for aquatic mosquito populations
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @inheritParams xds_compute_terms
#' @return a [list] containing the vector of all state derivatives
#' @export
xds_compute_terms.aquatic <- function(t, y, xds_obj) {
  # set the values of Exogenous forcing variables
  xds_obj <- Forcing(t, xds_obj)
  # set the values of resource variables
  xds_obj <- Resources(t, y, xds_obj)
  # vector control
  xds_obj <- VectorControl(t, y, xds_obj)
  # Compute baseline mosquito bionomic parameters
  xds_obj <- BaselineBionomics(t, y, xds_obj)
  # Compute total effect size assuming
  # "independent effect sizes"
  xds_obj <- VectorControlEffectSizes(t, y, xds_obj)
  # Modify the baseline
  xds_obj <- ModifiedBionomics(t, y, xds_obj)
  # egg laying: compute eta
  xds_obj$eggs_laid[[1]] = F_eggs(t, y, xds_obj, 1)
  return(xds_obj)  
}

#' @title Differential equation models for aquatic mosquito populations
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @inheritParams xds_compute_terms
#' @return a [list] containing the vector of all state derivatives
#' @export
xds_compute_terms.eir <- function(t, y, xds_obj) {
  # EIR: entomological inoculation rate trace
  xds_obj$terms$EIR[[1]] <- with(xds_obj$EIRpar, eir*F_season(t)*F_trend(t)*F_age(t)) 
  # FoI: force of infection
  xds_obj <- Exposure(t, y, xds_obj)
  return(xds_obj)  
}  
