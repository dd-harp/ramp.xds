# generalized spatial differential equations

#' @title Update States for Discrete-Time Systems
#' @description Updates the state variables
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @return a [list] containing the vector of all state derivatives
#' @export
dts_update <- function(t, y, pars) {
  UseMethod("dts_update", pars$frame)
}

#' @title Generalized spatial differential equation model
#' @description Update the state variables
#' @inheritParams dts_update
#' @return a [list] containing the vector of all state derivatives
#' @export
dts_update.full <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Abiotic(t, pars)
  pars <- Shock(t, pars)
  pars <- Control(t, y, pars)
  pars <- Behavior(t, y, pars)
  pars <- Visiting(t, pars)
  pars <- VectorControlEffects(t, y, pars)
  pars <- Resources(t, y, pars)

  # set and modify the baseline bionomic parameters
  pars <- Bionomics(t, y, pars)
  pars <- VectorControlEffectSizes(t, y, pars)

  # egg laying: compute eta
  pars <- EggLaying(t, y, pars)

  # emergence: compute Lambda
  pars <- Emergence(t, y, pars)

  # compute beta, EIR, and kappa
  pars <- Transmission(t, y, pars)

  # compute the FoI
  pars <- Exposure(t, y, pars)

  # Update Variables
  Lt <- Update_Lt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors)
      Lt <- c(Lt, Update_Lt(t, y, pars, s))

  MYZt <- Update_MYZt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors)
      MYZt <- c(MYZt, Update_MYZt(t, y, pars, s))

  Xt <- Update_Xt(t, y, pars, 1)
  if(pars$nHosts > 1)
    for(i in 2:pars$nHosts)
      Xt <- c(Xt, Update_Xt(t, y, pars, i))

  return(unlist(c(Lt, MYZt, Xt)))
}


#' @title Difference equations isolating the humans, forced with Ztrace
#' @description Compute and update the state variables for
#' a model with only humans
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @export
dts_update.human <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Abiotic(t, pars)
  pars <- Shock(t,  pars)
  pars <- Control(t, y, pars)
  pars <- Behavior(t, y, pars)
  pars <- Resources(t, y, pars)

  # set and modify the baseline mosquito bionomic parameters
  pars <- MBionomics(t, y, pars, 1)
  pars <- VectorControlEffectSizes(t, y, pars)

  # compute beta, EIR, and kappa
  pars <- Transmission(t, y, pars)

  # compute the FoI
  pars <- Exposure(t, y, pars)

  # state derivatives
  Xt <- Update_Xt(t, y, pars, 1)
  if(pars$nHosts > 1)
    for(i in 2:pars$nHosts)
      Xt <- c(Xt, Update_Xt(t, y, pars, i))

  return(c(Xt))
}

#' @title Generalized spatial differential equation model (mosquito only)
#' @description Compute and update the state variables for
#' a model with mosquito ecology (no transmission)
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @export
dts_update.mosy <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Abiotic(t, pars)
  pars <- Shock(t, pars)
  pars <- Control(t, y, pars)
  pars <- Behavior(t, y, pars)
  #pars <- Resources(t, y, pars)

  # set baseline mosquito bionomic parameters
  pars <- Bionomics(t, y, pars)
  pars <- VectorControlEffectSizes(t, y, pars)

  # egg laying: compute eta
  pars <- EggLaying(t, y, pars)

  # emergence: compute Lambda
  pars <- Emergence(t, y, pars)

  # compute derivatives
  Lt <- Update_Lt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors)
      Lt <- c(Lt, Update_Lt(t, y, pars, s))

  Mt <- Update_MYZt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors)
      Mt <- c(Mt, Update_MYZt(t, y, pars, s))

  return(c(Lt, Mt))
}

#' @title Difference equation models for human cohorts
#' @description Compute and update the state variables for
#' a cohort
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @export
dts_update.cohort <- function(t, y, pars) {

  # EIR: entomological inoculation rate trace
  pars$EIR[[1]] <- with(pars$EIRpar, F_eir(t, bday, scale))*pars$BFpar$relativeBitingRate[[1]][[1]]

  # FoI: force of infection
  pars <- Exposure(t, y, pars)

  # state derivatives
  Xt <- Update_Xt(t, y, pars, 1)
  if(pars$nHosts > 1)
    for(i in 2:pars$nHosts)
      Xt <- c(Xt, Update_Xt(t, y, pars, i))

  return(c(Xt))
}

#' @title Difference equation models for aquatic mosquito populations
#' @description Compute and update the state variables for
#' a model with only aquatic mosquitoes
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @export
dts_update.aquatic <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Abiotic(t, pars)
  pars <- Shock(t, pars)
  pars <- Control(t, y, pars)
  pars <- HabitatDynamics(t, pars)

  # modify baseline mosquito bionomic parameters
  pars <- LBionomics(t, y, pars, 1)
  pars <- VectorControlEffectSizes(t, y, pars)

  # egg laying: compute eta

  pars$eggs_laid[[1]] = F_eggs(t, y, pars, 1)

  # compute derivatives
  Lt <- Update_Lt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors)
      Lt <- c(Lt, Update_Lt(t, y, pars, s))

  return(c(Lt))
}
