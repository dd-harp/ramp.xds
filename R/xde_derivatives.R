# generalized spatial differential equations

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
#' @noRd
#' 
#' @export
xde_derivatives <- function(t, y, xds_obj) {
  UseMethod("xde_derivatives", xds_obj$frame)
}

#' @title Generalized spatial differential equation model
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' 
#' @inheritParams xde_derivatives
#' 
#' @noRd
#' 
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_derivatives.full <- function(t, y, xds_obj) {
  
  xds_obj <- xds_compute_terms(t, y, xds_obj)
  
  dL <- dLdt(t, y, xds_obj, 1)
  dMY <- dMYdt(t, y, xds_obj, 1)
  
  if(xds_obj$nVectorSpecies > 1)
    for(s in 2:xds_obj$nVectorSpecies){
      dL <- c(dL, dLdt(t, y, xds_obj, s))
      dMY <- c(dMY, dMYdt(t, y, xds_obj, s))
    }

  dXH <- dXHdt(t, y, xds_obj, 1)
  if(xds_obj$nHostSpecies > 1)
    for(i in 2:xds_obj$nHostSpecies)
      dXH <- c(dXH, dXHdt(t, y, xds_obj, i))
 
  dV <- dVdt(t, y, xds_obj, 1)
  if(xds_obj$nOtherVariables > 1)
    for(i in 2:length(xds_obj$nOtherVariables))
      dV <- c(dV, dVdt(t, y, xds_obj, i)) 
  
  return(list(c(dL, dMY, dXH, dV)))
}

#' @title Differential equations isolating the humans, forced with Ztrace
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @inheritParams xde_derivatives
#' 
#' @noRd
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_derivatives.human <- function(t, y, xds_obj) {
  
  xds_obj <- xds_compute_terms(t, y, xds_obj)
  
  dXH <- dXHdt(t, y, xds_obj, 1)
  if(xds_obj$nHostSpecies > 1)
    for(i in 2:xds_obj$nHostSpecies)
      dXH <- c(dXH, dXHdt(t, y, xds_obj, i))

  dV <- dVdt(t, y, xds_obj, 1)
  if(xds_obj$nOtherVariables > 1)
    for(i in 2:length(xds_obj$nOtherVariables))
      dV <- c(dV, dVdt(t, y, xds_obj, i))
  
  return(list(c(dXH, dV)))
}



#' @title Generalized spatial differential equation model (mosquito only)
#' @description Mirrors [ramp.xds::xde_derivatives] but only includes the adult and aquatic
#' mosquito components.
#' @inheritParams xde_derivatives
#' 
#' @noRd
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_derivatives.mosy <- function(t, y, xds_obj){
  
  xds_obj <- xds_compute_terms(t, y, xds_obj)
  
  dL <- dLdt(t, y, xds_obj, 1)
  dMY <- dMYdt(t, y, xds_obj, 1)
  
  if (xds_obj$nVectorSpecies > 1)
    for(s in 2:xds_obj$nVectorSpecies){
      dL <- c(dL, dLdt(t, y, xds_obj, s))
      dMY <- c(dMY, dMYdt(t, y, xds_obj, s))
    }

  dV <- dVdt(t, y, xds_obj, 1)
  if(xds_obj$nOtherVariables > 1)
    for(i in 2:length(xds_obj$nOtherVariables))
      dV <- c(dV, dVdt(t, y, xds_obj, i))

  return(list(c(dL, dMY, dV)))
}

#' @title Differential equation models for aquatic mosquito populations
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @inheritParams xde_derivatives
#' 
#' @noRd
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_derivatives.aquatic <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

  dL <- dLdt(t, y, xds_obj, 1)
  if(xds_obj$nVectorSpecies > 1)
    for(s in 1:xds_obj$nVectorSpecies)
      dL <- c(dL, dLdt(t, y, xds_obj, s))

  dV <- dVdt(t, y, xds_obj, 1)
  if(xds_obj$nOtherVariables > 1)
    for(i in 2:length(xds_obj$nOtherVariables))
      dV <- c(dV, dVdt(t, y, xds_obj, i))
  
  return(list(c(dL, dV)))
}

#' @title Differential equation models for aquatic mosquito populations
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @inheritParams xde_derivatives
#' 
#' @noRd
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_derivatives.eir <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)
  
  dXH <- dXHdt(t, y, xds_obj, 1)
  
  dV <- dVdt(t, y, xds_obj, 1)
  if(xds_obj$nOtherVariables > 1)
    for(i in 2:length(xds_obj$nOtherVariables))
      dV <- c(dV, dVdt(t, y, xds_obj, i))
  
  return(list(c(dXH, dV)))
}  
