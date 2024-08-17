
#' @title Compute other variables at time t
#' @description Compute everything but the derivatives for the generalized
#' spatial differential equation model
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @return **pars** a [list]
#' @export
reset_state <- function(t, y, pars) {
  UseMethod("reset_state", pars$frame)
}

#' @title Compute other variables at time t
#' @description Compute everything but the derivatives for the generalized
#' spatial differential equation model
#' @param i current simulation time
#' @param tm current simulation time
#' @param y_matrix state vector
#' @param pars a [list]
#' @return **pars** a [list]
#' @export
reset_state_i <- function(i, tm, y_matrix, pars) {
  pars <- reset_state(t=tm[i], y=y_matrix[i,], pars)
  return(pars)
}

#' @title Compute other variables at time t
#' @description Compute everything but the derivatives for the generalized
#' spatial differential equation model
#' @inheritParams reset_state
#' @return **pars** a [list]
reset_state.full <- function(t, y, pars) {
  pars <- Forcing(t, pars)
  pars <- Resources(t, pars)
  pars <- Control(t, y, pars)
  pars <- BloodFeeding(t, y, pars)
  pars <- EggLaying(t, y, pars)
  pars <- Bionomics(t, y, pars)
  pars <- VectorControlEffectSizes(t, y, pars)
  pars <- Emergence(t, y, pars)
  pars <- Transmission(t, y, pars)
  pars <- make_EIR_full(t, y, pars)
  pars <- Exposure(t, y, pars)
  return(pars)
}


#' @title Compute other variables at time t
#' @description Compute everything but the derivatives for the generalized
#' spatial differential equation model
#' @inheritParams reset_state
#' @return **pars** a [list]
reset_state.aquatic <- function(t, y, pars) {
  pars <- Forcing(t, pars)
  pars <- Resources(t, pars)
  pars <- Control(t, y, pars)
  pars <- Bionomics(t, y, pars)
  pars <- VectorControlEffectSizes(t, y, pars)
  return(pars)
}

#' @title Compute other variables at time t
#' @description Compute everything but the derivatives for the generalized
#' spatial differential equation model
#' @inheritParams reset_state
#' @return **pars** a [list]
reset_state.mosy <- function(t, y, pars) {
  pars <- Forcing(t, pars)
  pars <- Resources(t, pars)
  pars <- Control(t, y, pars)
  pars <- BloodFeeding(t, y, pars)
  pars <- EggLaying(t, y, pars)
  pars <- Bionomics(t, y, pars)
  pars <- VectorControlEffectSizes(t, y, pars)
  pars <- Emergence(t, y, pars)
  return(pars)
}

#' @title Compute other variables at time t
#' @description Compute everything but the derivatives for the generalized
#' spatial differential equation model
#' @inheritParams reset_state
#' @return **pars** a [list]
reset_state.human <- function(t, y, pars) {
  pars <- Forcing(t, pars)
  pars <- Resources(t, pars)
  pars <- Control(t, y, pars)
  pars <- BloodFeeding(t, y, pars)
  pars <- Bionomics(t, y, pars)
  pars <- VectorControlEffectSizes(t, y, pars)
  pars <- Transmission(t, y, pars)
  pars <- Exposure(t, y, pars)
  return(pars)
}

#' @title Compute other variables at time t
#' @description Compute everything but the derivatives for the generalized
#' spatial differential equation model
#' @inheritParams reset_state
#' @return **pars** a [list]
reset_state.cohort<- function(t, y, pars) {
  return(pars)
}
