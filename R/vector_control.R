
#' @title Implement Vector Control
#' @description The generic function to
#' implement vector control.
#' @note This a junction to implement various modes of
#' vector control.
#' Non-trivial vector control modules are in
#' [**`ramp.control`**](https://github.com/dd-harp/ramp.control).
#' @param t current simulation time
#' @param y state vector
#' @param pars an **`xds`** object
#' @return an **`xds`** object
VectorControl = function(t, y, pars){
  UseMethod("VectorControl", pars$vector_control)
}

#' @title Implement No Vector Control
#' @description With no vector control,
#' the **`xds`** object is returned unmodified
#' @note No control or `none` is the default setting
#' @inheritParams VectorControl
#' @return an **`xds`** object
VectorControl.none = function(t, y, pars){
  return(pars)
}


#' @title Implement Some Vector Control
#' @description Implements various forms
#' of vector control. Each mode for vector
#' control is set up and configured separately.
#' @note This a junction to implement various modes of
#' vector control.
#' Non-trivial vector control modules are in
#' [**`ramp.control`**](https://github.com/dd-harp/ramp.control).
#' @inheritParams VectorControl
#' @return a named [list]
#' @export
VectorControl.dynamic <- function(t, y, pars) {
  pars = BedNet(t, pars)
  pars = IRS(t, pars)
  pars = AreaSpray(t, pars)
  pars = SugarBaits(t, pars)
  pars = LSM(t, pars)
  #  pars = EM(t, pars)
  #  pars = Endectocide(t, pars)
  #  pars = ADLarvicide(t, pars)
  return(pars)
}

#' @title Vector Control for Static Vector Control
#' @description The `setup` case runs the `dynamic`
#' case once, to set the values of variables for
#' static models. It then reverts to `none` so that
#' those values are not changed again.
#' @inheritParams VectorControl
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
#' variables are configured so the function returns
#' the unmodified **`xds`** object
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
    pars <- AreaSprayEffectSizes(t, pars, s)
    pars <- SugarBaitEffectSizes(t, pars, s)
    pars <- LSMEffectSizes(t, pars, s)
  }
  #  pars = EM_EffectSizes(t, pars)
  #  pars = Endectocide_EffectSizes(t, pars)
  #  pars = ADLarvicide_EffectSizes(t, pars)
  return(pars)
}

#' @title Setup Function for No Vector Control (default)
#' @description This sets `class(vector_control) <- 'none'`
#' to dispatch the no vector control option.
#' @seealso [VectorControl.none]
#' @param pars an **`xds`** object
#' @return an **`xds`** object
setup_no_vector_control = function(pars){
  vector_control <- 'none'
  class(vector_control) <- 'none'
  pars$vector_control <- vector_control
  return(pars)
}

#' @title Turn On Vector Control
#' @description Any function that sets up
#' non-trivial vector control must
#' calls this function.
#' If `class(pars$vector_control) == 'none'`
#' then it is set to `dynamic` and the
#' trivial module for every mode of
#' vector control is set up. Otherwise,
#' nothing happens.
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_vector_control = function(pars){
  UseMethod("dynamic_vector_control", pars$vector_control)
}

#' @title Turn On Vector Control
#' @description If
#' `class(pars$vector_control) == 'none'`
#' then dynamic vector_control has not
#' been set up by any other function.
#' This sets `class(vector_control) <- 'dynamic'` and
#' then sets up a  trivial module for every mode of
#' vector control.
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_vector_control.none = function(pars){
  vector_control <- 'dynamic'
  class(vector_control) <- 'dynamic'
  pars$vector_control <- vector_control
  pars <- setup_no_bednets(pars)
  pars <- setup_no_irs(pars)
  pars <- setup_no_area_spray(pars)
  pars <- setup_no_lsm(pars)
  pars <- setup_no_sugar_baits(pars)
  return(pars)
}

#' @title Vector Control is Turned On
#' @description If
#' `class(pars$vector_control) == 'setup'`
#' then dynamic vector control has been turned
#' on. The unmodified **`xds`** object is returned.
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_vector_control.setup = function(pars){
  return(pars)
}

#' @title Vector Control is Turned On
#' @description If
#' `class(pars$vector_control) == 'dynamic'`
#' then dynamic vector control has been turned
#' on. The unmodified **`xds`** object is returned.
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_vector_control.dynamic = function(pars){
  return(pars)
}


