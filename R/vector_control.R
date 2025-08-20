
#' @title Implement Vector Control
#' @description The generic function to
#' implement vector control.
#' @note This a junction to implement various modes of
#' vector control.
#' Non-trivial vector control modules are in
#' [**`ramp.control`**](https://github.com/dd-harp/ramp.control).
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
VectorControl = function(t, y, xds_obj){
  UseMethod("VectorControl", xds_obj$vector_control)
}

#' @title Implement No Vector Control
#' @description With no vector control,
#' the **`xds`** object is returned unmodified
#' @note No control or `none` is the default setting
#' @inheritParams VectorControl
#' @return a **`ramp.xds`** model object
#' @export
VectorControl.none = function(t, y, xds_obj){
  return(xds_obj)
}


#' @title Setup Function for No Vector Control (default)
#' @description This sets `class(vector_control) <- 'none'`
#' to dispatch the no vector control option.
#' @seealso [VectorControl.none]
#' @param xds_obj an **`xds`** object
#' @return a **`ramp.xds`** model object
#' @export
setup_no_vector_control = function(xds_obj){
  vector_control <- 'none'
  class(vector_control) <- 'none'
  xds_obj$vector_control <- vector_control
  return(xds_obj)
}


#' @title Vector control effect sizes
#' @description This method dispatches on the type of `xds_obj$VECTOR_CONTROL`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** object
#' @return a **`ramp.xds`** model object
#' @export
VectorControlEffectSizes <- function(t, y, xds_obj) {
  UseMethod("VectorControlEffectSizes", xds_obj$vector_control)
}

#' @title Set the values of exogenous variables
#' @description After none setup, no exogenous
#' variables are configured so the function returns
#' the unmodified **`xds`** object
#' @inheritParams VectorControlEffectSizes
#' @return a **`ramp.xds`** model object
#' @export
VectorControlEffectSizes.none = function(t, y, xds_obj){
  return(xds_obj)
}