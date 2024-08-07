
#' @title Compute other variables at time t
#' @description Compute everything but the derivatives for the generalized
#' spatial differential equation model
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @return **pars** a [list]
#' @export
compute_vars_full <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Forcing(t, pars)
  pars <- Visiting(t, pars)

  # vector control
  pars <- Control(t, y, pars)

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

  return(pars)
}

#' @title Compute other variables at time t
#' @description Compute everything but the derivatives for a human-only
#' differential equation model
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @return **pars** a [list]
#' @export
compute_vars_human <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Forcing(t, pars)
  pars <- Visiting(t, pars)

  # vector control
  pars <- Control(t, y, pars)

  # blood feeding: available blood hosts, TaR, relative biting rates
  pars <- BloodFeeding(t, y, pars)

  # update adult bionomic parameters to baseline
  # or with integrated effect sizes
  pars <- MBionomics(t, y, pars, 1)

  if(pars$nVectors > 1) for(s in 2:pars$nVectors){
    pars <- MBionomics(t, y, pars, s)
  }
  pars <- VectorControlEffectSizes(t, y, pars)

  # emergence: compute Lambda
  pars <- Emergence(t, y, pars)

  return(pars)
}

#' @title Compute other variables at time t
#' @description Compute everything but the derivatives for a mosquito-only
#' differential equation model
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' the appropriate adult mosquito model
#' @return **pars** a [list]
#' @export
compute_vars_mosy <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Forcing(t, pars)
  pars <- Visiting(t, pars)

  # vector control
  pars <- Control(t, y, pars)

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
  return(pars)
}

#' @title Differential equation models for human cohorts
#' @description Compute everything but the derivatives for a human cohort
#' differential equation model
#' @param a age of a cohort
#' @param y state vector
#' @param pars a [list]
#' @param F_eir a trace function that returns the eir
#' @return **pars** a [list]
#' @export
compute_vars_cohort <- function(a, y, pars, F_eir) {

  # EIR: entomological inoculation rate trace
  pars$EIR[[1]] <- F_eir(a, pars)*pars$BFpar$relativeBitingRate[[1]][[1]]

  # FoI: force of infection
  pars <- Exposure(a, y, pars)

  return(pars)
}

#' @title Differential equation models for aquatic mosquito populations
#' @description Compute everything but the derivatives for an aquatic mosquito-only
#' differential equation model
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @return **pars** a [list]
#' @export
compute_vars_aqua <- function(t, y, pars) {

  # set the values of exogenous forcing variables
  pars <- Forcing(t, pars)

  # vector control
  pars <- Control(t, y, pars)

  # update adult bionomic parameters to baseline
  # or with integrated effect sizes
  pars <- LBionomics(t, y, pars, 1)

  if(pars$nVectors > 1) for(s in 2:pars$nVectors){
    pars <- LBionomics(t, y, pars, s)
  }
  pars <- VectorControlEffectSizes(t, y, pars)

  pars$eggs_laid[[1]] = F_eggs(t, y, pars, 1)

  return(pars)
}
