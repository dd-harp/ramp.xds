# generalized spatial differential equations

#' @title dts_update_ States for Discrete-Time Systems
#' @description dts_update_s the state variables
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @return a [list] containing the vector of all state derivatives
#' @export
dts_update <- function(t, y, pars) {
  UseMethod("dts_update", pars$frame)
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
dts_update_Lt = function(t, y, pars, s){
  tt = with(pars$runtime,runt(t,Dday,Lday))
  if(tt) return(Update_Lt(t, y, pars, s))
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
dts_update_MYZt = function(t, y, pars, s){
  tt = with(pars$runtime,runt(t,Dday,MYZday))
  if(tt) return(Update_MYZt(t, y, pars, s))
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
dts_update_Xt = function(t, y, pars, i){
  tt = with(pars$runtime,runt(t,Dday,Xday))
  if(tt) return(Update_Xt(t, y, pars, i))
  else return(list_Xvars(y, pars, i))
}

#' @title Generalized spatial differential equation model
#' @description dts_update_ the state variables
#' @inheritParams dts_update
#' @return a [list] containing the vector of all state derivatives
#' @export
dts_update.full <- function(t, y, pars) {
  # set the values of exogenous forcing variables
  # including malaria control coverage
  pars <- Forcing(t, y, pars)

  # blood feeding: available blood hosts, TaR, relative biting rates
  pars <- BloodFeeding(t, y, pars)

  # egg laying: available habitat, egg distribution matrix
  pars <- EggLaying(t, y, pars)

  # update adult bionomic parameters to baseline
  # or with integrated effect sizes
  pars <- MBionomics(t, y, pars, 1)
  pars <- LBionomics(t, y, pars, 1)

  if(pars$nVectors > 1) for(s in 2:pars$nVectors){
    pars <- MBionomics(t, y, pars, s)
    pars <- LBionomics(t, y, pars, s)
  }

  # modify mosquito bionomic parameters
  # by computing independent effect sizes
  pars <- VectorControlEffectSizes(t, y, pars)

  # emergence: Lambda
  pars <- Emergence(t, y, pars)

  # transmission: beta, EIR, and kappa
  pars <- Transmission(t, y, pars)

  # compute the FoI
  pars <- Exposure(t, y, pars)

  # dts_update_ Variables
  Lt <- dts_update_Lt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors)
      Lt <- c(Lt, dts_update_Lt(t, y, pars, s))

  MYZt <- dts_update_MYZt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors)
      MYZt <- c(MYZt, dts_update_MYZt(t, y, pars, s))

  Xt <- dts_update_Xt(t, y, pars, 1)
  if(pars$nHosts > 1)
    for(i in 2:pars$nHosts)
      Xt <- c(Xt, dts_update_Xt(t, y, pars, i))

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
  pars <- Forcing(t, y, pars)

  # blood feeding: available blood hosts, TaR, relative biting rates
  pars <- BloodFeeding(t, y, pars)

  # set and modify the baseline mosquito bionomic parameters
  pars <- MBionomics(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors)
      pars <- MBionomics(t, y, pars, s)

  pars <- VectorControlEffectSizes(t, y, pars)

  # compute beta, EIR, and kappa
  pars <- Transmission(t, y, pars)

  # compute the FoI
  pars <- Exposure(t, y, pars)

  # state derivatives
  Xt <- dts_update_Xt(t, y, pars, 1)
  if(pars$nHosts > 1)
    for(i in 2:pars$nHosts)
      Xt <- c(Xt, dts_update_Xt(t, y, pars, i))

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
  pars <- Forcing(t, y, pars)

  # blood feeding: available blood hosts, TaR, relative biting rates
  pars <- BloodFeeding(t, y, pars)

  # egg laying: available habitat, egg distribution matrix
  pars <- EggLaying(t, y, pars)

  # update adult bionomic parameters to baseline
  # or with integrated effect sizes
  pars <- MBionomics(t, y, pars, 1)
  pars <- LBionomics(t, y, pars, 1)

  if(pars$nVectors > 1) for(s in 2:pars$nVectors){
    pars <- MBionomics(t, y, pars, s)
    pars <- LBionomics(t, y, pars, s)
  }
  pars <- VectorControlEffectSizes(t, y, pars)

  # emergence: compute Lambda
  pars <- Emergence(t, y, pars)
  # compute derivatives
  Lt <- dts_update_Lt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors)
      Lt <- c(Lt, dts_update_Lt(t, y, pars, s))

  Mt <- dts_update_MYZt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors)
      Mt <- c(Mt, dts_update_MYZt(t, y, pars, s))

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
  Xt <- dts_update_Xt(t, y, pars, 1)
  if(pars$nHosts > 1)
    for(i in 2:pars$nHosts)
      Xt <- c(Xt, dts_update_Xt(t, y, pars, i))

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
  pars <- Forcing(t, y, pars)

  # modify baseline mosquito bionomic parameters
  pars <- LBionomics(t, y, pars, 1)
  pars <- VectorControlEffectSizes(t, y, pars)

  # egg laying: compute eta

  pars$eggs_laid[[1]] = F_eggs(t, y, pars, 1)

  # compute derivatives
  Lt <- dts_update_Lt(t, y, pars, 1)
  if(pars$nVectors > 1)
    for(s in 2:pars$nVectors)
      Lt <- c(Lt, dts_update_Lt(t, y, pars, s))

  return(c(Lt))
}
