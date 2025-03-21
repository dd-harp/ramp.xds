
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
#' @export
VectorControl = function(t, y, pars){
  UseMethod("VectorControl", pars$vector_control)
}

#' @title Implement No Vector Control
#' @description With no vector control,
#' the **`xds`** object is returned unmodified
#' @note No control or `none` is the default setting
#' @inheritParams VectorControl
#' @return an **`xds`** object
#' @export
VectorControl.none = function(t, y, pars){
  return(pars)
}


#' @title Setup Function for No Vector Control (default)
#' @description This sets `class(vector_control) <- 'none'`
#' to dispatch the no vector control option.
#' @seealso [VectorControl.none]
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_vector_control = function(pars){
  vector_control <- 'none'
  class(vector_control) <- 'none'
  pars$vector_control <- vector_control
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
#' @export
VectorControlEffectSizes.none = function(t, y, pars){
  return(pars)
}