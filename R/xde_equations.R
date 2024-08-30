# generalized spatial differential equations

#' @title Compute Derivatives
#' @description The function to compute the derivatives dispatches on `class(frame)`
#'
#' @seealso [xde_solve]
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_derivatives <- function(t, y, pars) {
  UseMethod("xde_derivatives", pars$frame)
}

#' @title Generalized spatial differential equation model
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @inheritParams xde_derivatives
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_derivatives.full <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Forcing(t, pars)
  # set the values of resource variables
  pars <- Resources(t, pars)
  # implement malaria control
  pars <- Control(t, y, pars)

  # blood feeding: available blood hosts, TaR, relative biting rates
  pars <- BloodFeeding(t, y, pars)
  # egg laying: available habitat, egg distribution matrix
  pars <- EggLaying(t, y, pars)

  # available sugar & bionomic parameters
  pars <- Bionomics(t, y, pars)
  # modify mosquito bionomic parameters
  # by computing independent effect sizes
  pars <- VectorControlEffectSizes(t, y, pars)

  # emergence: Lambda
  pars <- Emergence(t, y, pars)
  # transmission: beta, EIR, and kappa
  pars <- Transmission(t, y, pars)

  # compute the FoI
  pars <- Exposure(t, y, pars)

  # compute derivatives
  dL <- dLdt(t, y, pars, 1)
  dMYZ <- dMYZdt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors){
      dL <- c(dL, dLdt(t, y, pars, s))
      dMYZ <- c(dMYZ, dMYZdt(t, y, pars, s))
    }

  dX <- dXdt(t, y, pars, 1)
  if(pars$nHosts > 1)
    for(i in 2:pars$nHosts)
      dX <- c(dX, dXdt(t, y, pars, i))

  return(list(c(dL, dMYZ, dX)))
}

#' @title Differential equations isolating the humans, forced with Ztrace
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @inheritParams xde_derivatives
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_derivatives.human <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Forcing(t, pars)
  # set the values of resource variables
  pars <- Resources(t, pars)
  # vector control
  pars <- Control(t, y, pars)


  # blood feeding: available blood hosts, TaR, relative biting rates
  pars <- BloodFeeding(t, y, pars)

  # set and modify the baseline mosquito bionomic parameters
  pars <- Bionomics(t, y, pars)

  # modify the baseline mosquito bionomic parameters
  pars <- VectorControlEffectSizes(t, y, pars)

  # compute beta, EIR, and kappa
  pars <- Transmission(t, y, pars)

  # compute the FoI
  pars <- Exposure(t, y, pars)

  # state derivatives
  dX <- dXdt(t, y, pars, 1)
  if(pars$nHosts > 1)
    for(i in 2:pars$nHosts)
      dX <- c(dX, dXdt(t, y, pars, i))

  return(list(c(dX)))
}



#' @title Generalized spatial differential equation model (mosquito only)
#' @description Mirrors [ramp.xds::xde_derivatives] but only includes the adult and aquatic
#' mosquito components.
#' @inheritParams xde_derivatives
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_derivatives.mosy <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Forcing(t, pars)
  # set the values of resource variables
  pars <- Resources(t, pars)
  # vector control
  pars <- Control(t, y, pars)

  # blood feeding: available blood hosts, TaR, relative biting rates
  pars <- BloodFeeding(t, y, pars)

  # egg laying: available habitat, egg distribution matrix
  pars <- EggLaying(t, y, pars)

  # update adult bionomic parameters to baseline
  # or with integrated effect sizes
  pars <- Bionomics(t, y, pars)

  pars <- VectorControlEffectSizes(t, y, pars)

  # emergence: compute Lambda
  pars <- Emergence(t, y, pars)

  # state derivatives
  dL <- dLdt(t, y, pars, 1)
  dM <- dMYZdt(t, y, pars, 1)
  if (pars$nVectors > 1)
    for(s in 2:pars$nVectors){
      dL <- c(dL, dLdt(t, y, pars, s))
      dM <- c(dM, dMYZdt(t, y, pars, s))
    }

  return(list(c(dL, dM)))
}


#' @title Differential equation models for aquatic mosquito populations
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @inheritParams xde_derivatives
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_derivatives.aquatic <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Forcing(t, pars)
  # set the values of resource variables
  pars <- Resources(t, pars)
  # vector control
  pars <- Control(t, y, pars)

  # modify baseline mosquito bionomic parameters
  pars <- Bionomics(t, y, pars)
  pars <- VectorControlEffectSizes(t, y, pars)

  # egg laying: compute eta

  pars$eggs_laid[[1]] = F_eggs(t, y, pars, 1)

  # state derivatives
  dL <- dLdt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 1:pars$nVectors)
      dL <- c(dL, dLdt(t, y, pars, s))

  return(list(c(dL)))
}
