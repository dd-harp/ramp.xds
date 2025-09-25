
#' @title Setup a Junction for Vector Control 
#' 
#' @description This creates `xds_obj$vector_control` 
#' and sets `class(vector_control) = 'none'.` 
#' 
#' Functions to implement vector control are 
#' in a companion package called 
#' [**`ramp.control`**](https://github.com/dd-harp/ramp.control).
#' 
#' @seealso [VectorControl.none]
#' 
#' @param xds_obj an **`xds`** model object
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
setup_vector_control_object = function(xds_obj){
  vector_control <- list() 
  vector_control$name <- "Junction: Vector Control" 
  vector_control$ports <- "Ports: IRS, Bed Nets, LSM, Area Spraying (see ramp.control)" 
  vector_control$domore <- "see `ramp.control`" 
  class(vector_control) = "none"
  xds_obj$vector_control_obj <- vector_control
  return(xds_obj)
}

#' @title Vector Control
#' @description The generic function to
#' implement vector control.
#' @note This a junction to implement various modes of
#' vector control.
#' Non-trivial vector control modules are in
#' [**`ramp.control`**](https://github.com/dd-harp/ramp.control).
#' 
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' 
#' @return a **`ramp.xds`** model object
#' @export
VectorControl = function(t, y, xds_obj){
  UseMethod("VectorControl", xds_obj$vector_control_obj)
}

#' @title Compute Vector Control Effect Sizes
#' 
#' @description With no vector control,
#' the **`xds`** object is returned unmodified
#' 
#' @note No control or `none` is the default setting
#' 
#' @inheritParams VectorControl
#' 
#' @return a **`ramp.xds`** model object
#' @export
VectorControl.none = function(t, y, xds_obj){
  return(xds_obj)
}


#' @title Vector control effect sizes
#' @description This method dispatches on the type of `xds_obj$VECTOR_CONTROL`.
#' 
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' 
#' @return a **`ramp.xds`** model object
#' @export
VectorControlEffectSizes <- function(t, y, xds_obj) {
  UseMethod("VectorControlEffectSizes", xds_obj$vector_control_obj)
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
