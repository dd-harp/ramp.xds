# generalized spatial differential equations

#' @title Compute Derivatives
#' @description The function to compute the derivatives dispatches on `class(frame)`
#'
#' @seealso [xds_solve]
#' @param t current simulation time
#' @param y state vector
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Exogenous <- function(t, y, pars) {
  UseMethod("Exogenous", pars$frame)
}

#' @title Generalized spatial differential equation model
#' @description Compute exogenous forcing variables
#' @inheritParams Exogenous
#' @return a [list] containing the vector of all state derivatives
#' @export
Exogenous.full <- function(t, y, pars) {

  # set the values of Exogenous forcing variables
  pars <- Forcing(t, pars)
  # implement malaria control
  pars <- Health(t, y, pars)
  # implement vector control
  pars <- VectorControl(t, y, pars)
  # set the values of resource variables -- 
  pars <- Resources(t, pars)
  # blood feeding: available blood hosts, TaR, relative biting rates
  pars <- BloodFeeding(t, y, pars)
  # egg laying: available habitat, egg distribution matrix
  pars <- EggLaying(t, y, pars)
  # Compute baseline mosquito bionomic parameters
  # (including available sugar)
  pars <- BaselineBionomics(t, y, pars)
  # Compute  "independent effect sizes"
  pars <- VectorControlEffectSizes(t, y, pars)
  # Modify the baseline
  pars <- Bionomics(t, y, pars)

  return(pars)
}

#' @title Differential equations isolating the humans, forced with Ztrace
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @inheritParams Exogenous
#' @return a [list] containing the vector of all state derivatives
#' @export
Exogenous.human <- function(t, y, pars) {

  # weather port -- see ramp.forcing
  pars <- Forcing(t, pars)
  # malaria control port -- see ramp.control
  pars <- Health(t, y, pars)
  # vector control port -- see ramp.control
  pars <- VectorControl(t, y, pars)
  # set the values of resource variables
  pars <- Resources(t, pars)
  # blood feeding: available blood hosts, TaR, relative biting rates
  pars <- BloodFeeding(t, y, pars)
  # Compute baseline mosquito bionomic parameters
  # (including available sugar)
  pars <- BaselineBionomics(t, y, pars)
  # Compute total effect size assuming
  # "independent effect sizes"
  pars <- VectorControlEffectSizes(t, y, pars)
  # Modify the baseline
  pars <- Bionomics(t, y, pars)
  return(pars)
}



#' @title Generalized spatial differential equation model (mosquito only)
#' @description Mirrors [ramp.xds::Exogenous] but only includes the adult and aquatic
#' mosquito components.
#' @inheritParams Exogenous
#' @return a [list] containing the vector of all state derivatives
#' @export
Exogenous.mosy <- function(t, y, pars) {

  # set the values of Exogenous forcing variables
  pars <- Forcing(t, pars)
  # vector control
  pars <- VectorControl(t, y, pars)
  # set the values of resource variables
  pars <- Resources(t, pars)
  # blood feeding: available blood hosts, TaR, relative biting rates
  pars <- BloodFeeding(t, y, pars)
  # egg laying: available habitat, egg distribution matrix
  pars <- EggLaying(t, y, pars)
  # Compute baseline mosquito bionomic parameters
  # (including available sugar)
  pars <- BaselineBionomics(t, y, pars)
  # Compute total effect size assuming
  # "independent effect sizes"
  pars <- VectorControlEffectSizes(t, y, pars)
  # Modify the baseline
  pars <- Bionomics(t, y, pars)

  return(pars)
}


#' @title Differential equation models for aquatic mosquito populations
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @inheritParams Exogenous
#' @return a [list] containing the vector of all state derivatives
#' @export
Exogenous.aquatic <- function(t, y, pars) {

  # set the values of Exogenous forcing variables
  pars <- Forcing(t, pars)
  # set the values of resource variables
  pars <- Resources(t, pars)
  # vector control
  pars <- VectorControl(t, y, pars)

  # Compute baseline mosquito bionomic parameters
  pars <- BaselineBionomics(t, y, pars)
  # Compute total effect size assuming
  # "independent effect sizes"
  pars <- VectorControlEffectSizes(t, y, pars)
  # Modify the baseline
  pars <- Bionomics(t, y, pars)
  return(pars)
}
