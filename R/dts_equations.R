# generalized spatial differential equations


#' @title Generalized spatial differential equation model
#' @description Compute and update the state variables for
#' the generic model
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @return a [list] containing the vector of all state derivatives
#' @export
DTS_step <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Abiotic(t, pars)
  pars <- Shock(t, pars)
  pars <- Control(t, y, pars)
  pars <- Behavior(t, y, pars)
  pars <- Visitors(t, pars)
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

  # compute derivatives
  if((t%%pars$Lday +1) == 1){
    Lt <- DT_Lt(t, y, pars, 1)
    if(pars$nVectors > 1)
      for(s in 2:pars$nVectors)
        Lt <- c(Lt, DT_Lt(t, y, pars, s))
  }

  if((t%%pars$MYZday+1) == 1){
    MYZt <- DT_MYZt(t, y, pars, 1)
    if(pars$nVectors > 1)
      for(s in 2:pars$nVectors)
        MYZt <- c(MYZt, DT_MYZt(t, y, pars, s))
  }

  if((t%%pars$Xday+1) == 1){
    Xt <- DT_Xt(t, y, pars, 1)
    if(pars$nHosts > 1)
      for(i in 2:pars$nHosts)
        Xt <- c(Xt, DT_Xt(t, y, pars, i))
  }

  return(c(Lt, MYZt, Xt))
}

#' @title Difference equations isolating the humans, forced with Ztrace
#' @description Compute and update the state variables for
#' a model with only humans
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @return a [vector] containing the vector of all state derivatives
#' @export
DTS_step_human <- function(t, y, pars) {

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
  if((t%%pars$Xday+1) == 1){
    Xt <- DT_Xt(t, y, pars, 1)
    if(pars$nHosts > 1)
      for(i in 2:pars$nHosts)
        Xt <- c(Xt, DT_Xt(t, y, pars, i))
  }

  return(c(Xt))
}



#' @title Generalized spatial differential equation model (mosquito only)
#' @description Compute and update the state variables for
#' a model with mosquito ecology (no transmission)
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' the appropriate adult mosquito model
#' @return a [vector] containing the vector of all state derivatives
#' @export
DTS_step_mosy <- function(t, y, pars) {

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
  if((t%%pars$Lday +1) == 1){
    Lt <- DT_Lt(t, y, pars, 1)
    if(pars$nVectors > 1)
      for(s in 2:pars$nVectors)
        Lt <- c(Lt, DT_Lt(t, y, pars, s))
  }

  if((t%%pars$MYZday+1) == 1){
    MYZt <- DT_MYZt(t, y, pars, 1)
    if(pars$nVectors > 1)
      for(s in 2:pars$nVectors)
        MYZt <- c(MYZt, DT_MYZt(t, y, pars, s))
  }

  return(c(Lt, Mt))
}

#' @title Difference equation models for human cohorts
#' @description Compute and update the state variables for
#' a cohort
#' @param a age of a cohort
#' @param y state vector
#' @param pars a [list]
#' @param F_eir a trace function that returns the eir as a function of time
#' @return a [vector] containing the vector of all state derivatives
#' @export
DTS_step_cohort <- function(a, y, pars, F_eir) {

  # EIR: entomological inoculation rate trace
  pars$EIR[[1]] <- with(pars$EIRpar, F_eir(a, bday, scale))*pars$BFpar$relativeBitingRate[[1]][[1]]

  # FoI: force of infection
  pars <- Exposure(a, y, pars)

  # state derivatives
  if((t%%pars$Xday+1) == 1){
    Xt <- DT_Xt(t, y, pars, 1)
    if(pars$nHosts > 1)
      for(i in 2:pars$nHosts)
        Xt <- c(Xt, DT_Xt(t, y, pars, i))
  }

  return(c(Xt))
}

#' @title Difference equation models for aquatic mosquito populations
#' @description Compute and update the state variables for
#' a model with only aquatic mosquitoes
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @return a [vector] containing the vector of all state derivatives
#' @export
DTS_step_aquatic <- function(t, y, pars) {

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
  if((t%%pars$Lday +1) == 1){
    Lt <- DT_Lt(t, y, pars, 1)
    if(pars$nVectors > 1)
      for(s in 2:pars$nVectors)
        Lt <- c(Lt, DT_Lt(t, y, pars, s))
  }

  return(c(Lt))
}
