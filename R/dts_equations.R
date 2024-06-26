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


#' @title A runtime switch function for mismatched dynamical component runtimes
#' @description Determine whether to update the variables at time t
#' @param t current simulation time
#' @param Dday the runtime time step for the simulation
#' @param xday the runtime time step for a component
#' @return [logical] TRUE or FALSE
#' @export
runt = function(t, Dday, xday){
  t1 = round(t/Dday)
  t2 = round(xday/Dday)
  t1 %% t2 < 1e-2
}

#' @title Difference equations isolating the humans, forced with Ztrace
#' @description Compute and update the state variables for
#' a model with only humans
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the vector species index
#' @return a [vector] containing the vector of all state derivatives
#' @export
Update_Lt = function(t, y, pars, s){
  if(runt(t,pars$Dday,pars$Lday)) return(DT_Lt(t, y, pars, s))
  else return(list_Lvars(y, pars,s))
}

#' @title Difference equations isolating the humans, forced with Ztrace
#' @description Compute and update the state variables for
#' a model with only humans
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the vector species index
#' @return a [vector] containing the vector of all state derivatives
#' @export
Update_MYZt = function(t, y, pars, s){
  if(runt(t, pars$Dday, pars$MYZday)) return(DT_MYZt(t, y, pars, s))
  else return(list_MYZvars(y, pars, s))
}

#' @title Difference equations isolating the humans, forced with Ztrace
#' @description Compute and update the state variables for
#' a model with only humans
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param i the host species index
#' @return a [vector] containing the vector of all state derivatives
#' @export
Update_Xt = function(t, y, pars, i){
  if(runt(t, pars$Dday, pars$Xday)) return(DT_Xt(t, y, pars, i))
  else return(list_Xvars(y, pars, i))
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
  Xt <- Update_Xt(t, y, pars, 1)
  if(pars$nHosts > 1)
    for(i in 2:pars$nHosts)
      Xt <- c(Xt, Update_Xt(t, y, pars, i))

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
  Xt <- Update_Xt(t, y, pars, 1)
  if(pars$nHosts > 1)
    for(i in 2:pars$nHosts)
      Xt <- c(Xt, Update_Xt(t, y, pars, i))

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
  Lt <- Update_Lt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors)
      Lt <- c(Lt, Update_Lt(t, y, pars, s))

  return(c(Lt))
}
