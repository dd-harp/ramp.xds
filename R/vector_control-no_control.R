# specialized methods for the no_control model of vector control

#' @title Distribute vector control
#' @description Implements [VectorControl] for the no_control model of vector control (do nothing)
#' @inheritParams VectorControl
#' @return a named [list]
#' @export
VectorControl.no_control <- function(t, y, pars) {
  return(pars)
}

#' @title Vector control: durability & effects
#' @description Implements [VectorControlEffects] for the no_control model of vector control (do nothing)
#' @inheritParams VectorControlEffects
#' @return a named [list]
#' @export
VectorControlEffects.no_control <- function(t, y, pars) {
  return(pars)
}

#' @title Vector control effect sizes
#' @description Implements [VectorControlEffectSizes] for the no_control model of vector control (do nothing)
#' @inheritParams VectorControlEffectSizes
#' @return a named [list]
#' @export
VectorControlEffectSizes.no_control <- function(t, y, pars) {
  return(pars)
}

#' @title Distribute vector control, the no_control model
#' @param pars a [list]
#' @return no_control
#' @export
setup_vc_no_control <- function(pars) {
  VECTOR_CONTROL <- list()
  class(VECTOR_CONTROL) <- 'no_control'
  pars$VECTOR_CONTROL <- VECTOR_CONTROL
  return(pars)
}
