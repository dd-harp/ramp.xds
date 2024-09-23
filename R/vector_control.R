

#' @title Set the values of exogenous variables
#' @description Set the values of
#' exogenous variables.
#' @param t current simulation time
#' @param y state vector
#' @param pars an **`xds`** object
#' @return an **`xds`** object
VectorControl = function(t, y, pars){
  UseMethod("VectorControl", pars$vector_control)
}

#' @title Set the values of exogenous variables
#' @description After none setup, no exogenous
#' variables are configured so vector_control returns
#' the **`xds`** object without modification
#' @inheritParams VectorControl
#' @return an **`xds`** object
VectorControl.none = function(t, y, pars){
  return(pars)
}


#' @title Distribute vector control, the null model
#' @description Implements [VectorControl] for the control model of vector control (do nothing)
#' @inheritParams VectorControl
#' @return a named [list]
#' @export
VectorControl.dynamic <- function(t, y, pars) {
  pars = BedNet(t, pars)
  pars = IRS(t, pars)
  #  pars = AreaSpray(t, pars)
  #  pars = SugarBait(t, pars)
  #  pars = LSM(t, pars)
  #  pars = EM(t, pars)
  #  pars = Endectocide(t, pars)
  #  pars = ADLarvicide(t, pars)
  return(pars)
}

#' @title The `setup` case for exogenous vector_control
#' @description Call all the functions to set the
#' values of exogenous variables and then revert
#' the `none` case
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
VectorControl.setup = function(t, pars){
  class(pars$vector_control) <- 'dynamic'
  pars <- VectorControl(t, pars)
  class(pars$vector_control) <- 'none'
  return(pars)
}


#' @title Vector control effect sizes
#' @description This method dispatches on the type of `pars$VECTOR_CONTROL`.
#' @param t current simulation time
#' @param y state vector
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
VectorControlEffectSizes <- function(t, y, pars) {
  UseMethod("VectorControlEffectSizes", pars$vector_control)
}

#' @title Set the values of exogenous variables
#' @description After none setup, no exogenous
#' variables are configured so vector_control returns
#' the **`xds`** object without modification
#' @inheritParams VectorControlEffectSizes
#' @return an **`xds`** object
VectorControlEffectSizes.none = function(t, y, pars){
  return(pars)
}

#' @title Distribute vector control, the null model
#' @description Implements [VectorControl] for the control model of vector control (do nothing)
#' @inheritParams VectorControlEffectSizes
#' @return a named [list]
#' @export
VectorControlEffectSizes.dynamic <- function(t, y, pars) {
  for(s in 1:pars$nVectors){
    pars <- BedNetEffectSizes(t, pars, s)
    pars <- IRSEffectSizes(t, pars, s)
  }
  #  pars = AreaSprayEffectSizes(t, pars)
  #  pars = SugarBaitEffectSizes(t, pars)
  #  pars = LSMEffectSizes(t, pars)
  #  pars = EM_EffectSizes(t, pars)
  #  pars = Endectocide_EffectSizes(t, pars)
  #  pars = ADLarvicide_EffectSizes(t, pars)
  return(pars)
}

#' @title none set up for exogenous vector_control
#' @description This sets up the `none` option
#' for exogenous vector_control: no vector_control.
#' @param pars an **`xds`** object
#' @return an **`xds`** object
setup_no_vector_control = function(pars){
  vector_control <- 'none'
  class(vector_control) <- 'none'
  pars$vector_control <- vector_control
  return(pars)
}

#' @title Set up dynamic vector_control
#' @description If dynamic vector_control has not
#' already been set up, then turn on dynamic
#' vector_control and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_vector_control = function(pars){
  UseMethod("dynamic_vector_control", pars$vector_control)
}

#' @title Set up dynamic vector_control
#' @description If dynamic vector_control has not
#' already been set up, then turn on dynamic
#' vector_control and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_vector_control.none = function(pars){
  vector_control <- 'dynamic'
  class(vector_control) <- 'dynamic'
  pars$vector_control <- vector_control
  pars <- setup_no_bednets(pars)
  pars <- setup_no_irs(pars)
  return(pars)
}

#' @title Set up dynamic vector_control
#' @description If dynamic vector_control has not
#' already been set up, then turn on dynamic
#' vector_control and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_vector_control.setup = function(pars){
  return(pars)
}

#' @title Set up dynamic vector_control
#' @description If dynamic vector_control has not
#' already been set up, then turn on dynamic
#' vector_control and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_vector_control.dynamic = function(pars){
  return(pars)
}


