
#' @title Setup the Vector Control Junction
#' 
#' @description This creates `xds_obj$vector_control` 
#' and sets `class(vector_control) = 'none'.` 
#' 
#' Vector control is implemented in two stages.
#' 
#' Functions to implement vector control are 
#' in a companion package called 
#' [**`ramp.control`**](https://github.com/dd-harp/ramp.control).
#' 
#' @seealso [VectorControl1.none]
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

#' @title Vector Control, Stage 1
#' 
#' @description A junction to implement 
#' stage one vector control functions. 
#' 
#' @note 
#' Non-trivial vector control modules are in
#' [**`ramp.control`**](https://github.com/dd-harp/ramp.control).
#' 
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' 
#' @return a **`ramp.xds`** model object
#' @export
VectorControl1 = function(t, y, xds_obj){
  UseMethod("VectorControl1", xds_obj$vector_control_obj)
}

#' @title Compute Vector Control Effect Sizes
#' 
#' @description Do no vector control:
#' the **`xds`** object is returned unmodified
#' 
#' @note No control or `none` is the default setting
#' 
#' @inheritParams VectorControl1
#' 
#' @return a **`ramp.xds`** model object
#' @export
VectorControl1.none = function(t, y, xds_obj){
  return(xds_obj)
}


#' @title Vector Control, Stage 2 

#' @description A junction to implement 
#' stage two vector control functions 
#' 
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' 
#' @return a **`ramp.xds`** model object
#' @export
VectorControl2 <- function(t, y, xds_obj) {
  UseMethod("VectorControl2", xds_obj$vector_control_obj)
}

#' @title Set the values of exogenous variables
#' 
#' @description Do no vector control:
#' the **`xds`** object is returned unmodified
#' 
#' @inheritParams VectorControl2
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
VectorControl2.none = function(t, y, xds_obj){
  return(xds_obj)
}
