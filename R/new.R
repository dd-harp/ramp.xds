
#' @title Set the values of exogenous variables
#' @description Set the values of
#' exogenous variables.
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
Control = function(t, pars){
  UseMethod("Control", pars$control)
}

#' @title Set the values of exogenous variables
#' @description After basic setup, no exogenous
#' variables are configured so control returns
#' the **`xds`** object without modification
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
Control.basic = function(t, pars){
  return(pars)
}

#' @title Basic set up for exogenous control
#' @description This sets up the `basic` option
#' for exogenous control: no control.
#' @param pars an **`xds`** object
#' @return an **`xds`** object
setup_no_control = function(pars){
  control <- 'basic'
  class(control) <- 'basic'
  pars$control <- control
  return(pars)
}

#' @title The `setup` case for exogenous control
#' @description Call all the functions to set the
#' values of exogenous variables and then revert
#' the `basic` case
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
Control.setup = function(t, pars){
  class(pars$control) <- 'dynamic'
  pars <- Control(t, pars)
  class(pars$control) <- 'basic'
  return(pars)
}

#' @title Set the values of exogenous variables
#' @description With dynamic control, exogenous variables
#' can be set in one of four function calls:
#' - Weather
#' - Hydrology
#' - Shock
#' - Development
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @seealso [dynamic_control]
Control.dynamic = function(t, pars){
  pars <- UseBedNet(t, y, pars)
  pars <- CareSeeking(t, y, pars)
  pars <- MassMedical(t, y, pars)
  pars <- Clinic(t, y, pars)
  pars <- VectorControl(t, y, pars)
  pars <- VectorControlEffects(t, y, pars)
  return(pars)
}

#' @title Set up dynamic control
#' @description If dynamic control has not
#' already been set up, then turn on dynamic
#' control and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
dynamic_control = function(pars){
  UseMethod("dynamic_control", pars$control)
}

#' @title Set up dynamic control
#' @description If dynamic control has not
#' already been set up, then turn on dynamic
#' control and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_control.basic = function(pars){
  control <- 'dynamic'
  class(control) <- 'dynamic'
  pars$control <- control
  pars <- setup_habitat_dynamics_static(pars)
  pars <-  setup_care_seeking_no_behavior(pars)
  pars <-  setup_mass_medical_no_control(pars)
  pars <-  setup_clinic_no_control(pars)
  pars <-  setup_vc_no_control(pars)
  return(pars)
}

#' @title Set up dynamic control
#' @description If dynamic control has not
#' already been set up, then turn on dynamic
#' control and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_control.setup = function(pars){
  return(pars)
}

#' @title Set up dynamic control
#' @description If dynamic control has not
#' already been set up, then turn on dynamic
#' control and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_control.dynamic = function(pars){
  return(pars)
}


